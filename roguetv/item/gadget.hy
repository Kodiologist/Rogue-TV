(require kodhy.macros)

(import
  [random [randrange]]
  [heidegger.pos [Pos]]
  [kodhy.util [cat ret retf]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [input-direction]]
  [roguetv.map [Tile Floor Door on-map room-for? mset ray-taxi disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature]])

(defclass Gadget [Item] [
  [max-charges 10]
  [apply-time 1]
  [char "/"]

  [info-unidentified "This is some bizarre gizmo from a late-night infomercial included in Rogue TV as product placement. Goodness knows what it does; it could as easily be a soldering iron as a waffle iron. 'a'pply it to use it and find out. Each use will consume one of a limited number of charges."]

  [__init__ (fn [self &optional charges &kwargs rest]
    (apply Item.__init__ [self] rest)
    (setv self.charges (if (none? charges) self.max-charges charges))
    None)]

  [display-name (fn [self]
    (cat
      (.display-name (super Gadget self))
      (and self.appearance.known
        (.format " ({})" self.charges))))]

  [applied (fn [self] (block
    ; Do we have a charge to spare?
    (when (= self.charges 0)
      (msg :bob "That oojah's all chatty, kemosabe.")
      (ret))
    ; Identify the item type.
    (unless self.appearance.known
      (setv self.appearance.known True)
      (msg "You have:  {}" (self.invstr)))
    ; Now you get the gadget effect.
    (self.gadget-effect)))]

  [gadget-effect (fn [self]
    ; Do whatever the gadget should do. If the user ends up
    ; really getting the gadget effect (e.g., they don't cancel out
    ; of a direction prompt), be sure to call .use-time-and-charge
    ; before otherwise affecting the game world.
    (.use-time-and-charge self)
    (msg "Nothing happens."))]

  [use-time-and-charge (fn [self]
    ; Use up a charge and some time.
    (G.player.take-time self.apply-time)
    (-= self.charges 1))]])

(def appearances {
  "crazy"           :blue
  "cryptic"         :blue
  "mysterious"      :blue
  "enigmatic"       :blue
  "Art Deco"        :blue
  "clockwork"       :blue
  "futuristic"      :blue
  "alien"           :blue
  "modern"          :blue
  "shiny"           :blue
  "rusty"           :red
  "antique"         :blue
  "vintage"         :blue
  "ivory"           :white
  "wooden"          :brown
  "brass"           :yellow
  "silvery"         :dark-gray
  "stainless-steel" :dark-gray
  "matte"           :blue
  "flimsy"          :blue
  "rugged"          :blue
  "plastic"         :blue
  "tiny"            :blue
  "boxy"            :blue
  "sleek"           :blue
  "bulky"           :blue
  "crude"           :blue})
(setv appearances (dict (lc [[name color] (.items appearances)]
  (, name (kwc ItemAppearance
    :name (NounPhrase (+ name " gadget"))
    :color-fg color)))))

(defn randomize-appearances []
  (setv unused-appearances (.values appearances))
  (for [itype (filt (issubclass it Gadget) (.values G.itypes))]
    (.set-appearance itype (randpop unused-appearances))))

(def-itemtype Gadget "panic-button" :name "panic button"
  :info-flavor "Press it if you expect to be particularly lucky in the future, or if you are particularly unlucky in the present."
  :teleport-tries 100

  :info-apply "Teleports you to a random square elsewhere on the current dungeon level."
  :gadget-effect (fn [self] (block :gadget

    ; Find a place to teleport to.
    (block
      (for [_ (range self.teleport-tries)]
        (setv p-to (Pos (randrange G.map-width) (randrange G.map-height)))
        (when (and
            (!= p-to G.player.pos)
            (room-for? G.player p-to)
            (instance? Floor (Tile.at p-to)))
          (ret)))
      ; We failed to find a legal square.
      (.use-time-and-charge self)
      (msg "You feel cramped.")
      (retf :gadget))

    ; Now teleport there.
    (.use-time-and-charge self)
    (.move G.player p-to)
    (msg :tara "{p:He's} teleported to another part of the level."))))

(def-itemtype Gadget "warpback" :name "warpback machine"
  ; Has an extra instance attribute .warpback-pos.
  :info-flavor "It's deja vu all over again."

  :on-reset-level (fn [self]
    (setv self.warpback-pos None))

  :info-apply "Use it once to register a warpback point (for you, sir, no charge). Use it again to teleport back to the warpback point. This will clear the warpback point, as will going to another dungeon level."
  :gadget-effect (fn [self]

    (if (getattr self "warpback_pos" None)
      (do
        (.use-time-and-charge self)
        (if (room-for? G.player self.warpback-pos)
          (do
            (msg "You reappear at {:the}'s registered location." self)
            (.move G.player self.warpback-pos)
            (setv self.warpback-pos None))
          (msg "{:The} beeps at you accusingly." self)))
      (do
        (setv self.warpback-pos G.player.pos)
        (msg "{:The} registers your current position." self)))))

(def-itemtype Gadget "hookshot"
  :info-flavor "Arfer's law of game design: any video game is improved by the addition of a grappling hook."
  :hookshot-dist 8
  :hookshot-travel-speed 2

  :info-apply "Fire it at a solid obstacle up to {hookshot_dist} squares away to get yourself over there. Travel by hookshot is twice as fast as travel by foot, and you'll pass over unpleasant terrain along the way. Creatures will block the hookshot."
  :gadget-effect (fn [self] (block :gadget

    (setv d (or (input-direction) (ret)))

    ; Find our destination square.
    (setv ahead (+ G.player.pos d))
    (setv path (kwc ray-taxi ahead d self.hookshot-dist
      :+include-off-map))
    (block
      (for [p path]
        (unless (on-map p)
          (ret))
        (when (. (Tile.at p) blocks-movement)
          (ret))
        (whenn (Creature.at p)
          (.use-time-and-charge self)
          (msg :tara "{p:The}'s {} bounces off {:the}."
            self it)
          (retf :gadget)))
      (msg "Your {} can only reach objects up to {} squares away."
        self self.hookshot-dist)
      (retf :gadget))
    (setv p-to (- p d))
    (when (= p-to G.player.pos)
      (msg :tara "It looks like {p:the}'s {} isn't very useful at that range."
        self)
      (retf :gadget))

    ; And away we go.
    (.use-time-and-charge self)
    (.take-time G.player (/ (len-taxi (- p-to G.player.pos)) self.hookshot-travel-speed))
    (.move G.player p-to)
    (msg "{:The} pulls you ahead." self))))

(def-itemtype Gadget "chainsaw"
  :info-flavor "Just what you need to kickstart a lucrative career in lumberjacking after winning gobs of dosh on Rogue TV. Or you could sell it for an additional gob of dosh. Whatever; I don't judge."

  :info-apply "Instantly destroys an adjacent door."
  :gadget-effect (fn [self] (block

    (setv d (or (input-direction) (ret)))
    (setv p (+ G.player.pos d))
    (setv t (when (on-map p) (Tile.at p)))

    (if (instance? Door t)
      (do
        (.use-time-and-charge self)
        (msg "Bzzt! The door is no more.")
        (mset p (Floor)))
      (msg "Your {} won't help with that." self)))))

(def-itemtype Gadget "gps" :name "GPS device"
  :info-flavor "They say it's unwise to use a GPS device as your only means of navigation in an unfamiliar area, but it's not as if you have lots of better options in a dungeon."
  :gps-range 10

  :info-apply "Reveals the map in a radius of {gps_range} squares around you."
  :gadget-effect (fn [self]

    (.use-time-and-charge self)
    (for [p (disc-taxi G.player.pos self.gps-range)]
      (setv (get G.seen-map p.x p.y) True))
    (soil-fov)
    (msg "{:The} reveals part of the dungeon around you." self)))

(assert (>= (len appearances)
  (len (filt (instance? Gadget it) (.values G.itypes)))))
