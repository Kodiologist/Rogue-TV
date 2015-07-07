(require kodhy.macros roguetv.macros)

(import
  [random [randrange choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret retf]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [input-direction]]
  [roguetv.map [Tile Floor Door on-map room-for? mset mget ray-taxi disc-taxi]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature]])

(defclass Gadget [Item] [
  [max-charges 10]
  [apply-time 1]
  [char "/"]

  [info-unidentified "This is some bizarre gizmo from a late-night infomercial included in Rogue TV as product placement. Goodness knows what it does; it could as easily be a soldering iron as a waffle iron. 'a'pply it to use it and find out. Each use will consume one of a limited number of charges. If the gadget can be aimed or otherwise controlled, you'll only be able to control it once you know what it is."]

  [__init__ (fn [self &optional charges &kwargs rest]
    (apply Item.__init__ [self] rest)
    (setv self.charges (if (none? charges) self.max-charges charges))
    None)]

  [name-suffix (fn [self]
    (when (.identified? self)
      (.format "({})" self.charges)))]

  [applied (fn [self] (block
    ; Do we have a charge to spare?
    (when (= self.charges 0)
      (msg :bob "That oojah's all chatty, kemosabe.")
      (ret))
    ; Identify the item type.
    (setv unid (not (.identified? self)))
    (.identify self)
    ; Now you get the gadget effect.
    (self.gadget-effect unid)))]

  [gadget-effect (fn [self unid]
    ; Do whatever the gadget should do. `unid` says whether
    ; the item was unidentified before this use.
    ;
    ; If the user ends up really getting the gadget effect (they
    ; don't, e.g., cancel out of a direction prompt), be sure to
    ; call .use-time-and-charge before otherwise affecting the
    ; game world.
    (.use-time-and-charge self)
    (msg "Nothing happens."))]

  [use-time-and-charge (fn [self]
    ; Use up a charge and some time.
    (G.player.take-time self.apply-time)
    (-= self.charges 1))]])

(def appearances {
  "crazy"           :green
  "cryptic"         :dark-green
  "mysterious"      :purple
  "enigmatic"       :red
  "Art Deco"        :orange
  "clockwork"       :yellow
  "futuristic"      :purple
  "alien"           :green
  "modern"          :black
  "shiny"           :blue
  "rusty"           :orange
  "antique"         :dark-red
  "vintage"         :dark-blue
  "ivory"           :white
  "wooden"          :brown
  "brass"           :yellow
  "silvery"         :dark-gray
  "stainless-steel" :dark-gray
  "matte"           :black
  "flimsy"          :dark-orange
  "rugged"          :brown
  "plastic"         :red
  "tiny"            :white
  "boxy"            :black
  "sleek"           :blue
  "bulky"           :dark-green
  "crude"           :brown})
(setv (get ItemAppearance.registry Gadget) (lc
  [[name color] (.items appearances)]
  (kwc ItemAppearance
    :name (NounPhrase (+ name " gadget"))
    :color-fg color)))

(def-itemtype Gadget "panic-button" :name "panic button"
  :price 50
  :info-flavor "Press it if you expect to be particularly lucky in the future, or if you are particularly unlucky in the present."
  :teleport-tries 100

  :info-apply "Teleports you to a random square elsewhere on the current dungeon level."
  :gadget-effect (fn [self unid] (block :gadget

    (.use-time-and-charge self)

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
      (msg "You feel cramped.")
      (retf :gadget))

    ; Now teleport there.
    (.move G.player p-to)
    (msg :tara "{p:He's} teleported to another part of the level."))))

(def-itemtype Gadget "warpback" :name "warpback machine"
  :price 50
  ; Has an extra instance attribute .warpback-pos.
  :info-flavor "It's deja vu all over again."

  :on-reset-level (fn [self]
    (setv self.warpback-pos None))

  :info-apply "Use it once to register a warpback point (for you, sir, no charge). Use it again to teleport back to the warpback point. This will clear the warpback point, as will going to another dungeon level."
  :gadget-effect (fn [self unid]

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
  :price 30
  :info-flavor "Arfer's law of game design: any video game is improved by the addition of a grappling hook."
  :hookshot-dist 8
  :hookshot-travel-speed 2

  :info-apply "Fire it at a solid obstacle up to {hookshot_dist} squares away to get yourself over there. Travel by hookshot is twice as fast as travel by foot, and you'll pass over unpleasant terrain along the way. Creatures will block the hookshot."
  :gadget-effect (fn [self unid] (block :gadget

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (.use-time-and-charge self)

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
          (msg :tara "{p:The}'s {} bounces off {:the}."
            self it)
          (retf :gadget)))
      (msg "{:Your} doesn't hit anything. It can only reach objects up to {} squares away."
        self self.hookshot-dist)
      (retf :gadget))
    (setv p-to (- p d))
    (when (= p-to G.player.pos)
      (msg "{:Your} uselessly hits {:the} right next to you."
        self (mget ahead))
      (retf :gadget))

    ; And away we go.
    (.take-time G.player (/ (len-taxi (- p-to G.player.pos)) self.hookshot-travel-speed))
    (.move G.player p-to)
    (msg "{:The} pulls you ahead." self))))

(def-itemtype Gadget "chainsaw"
  :price 10
  :info-flavor "Just what you need to kickstart a lucrative career in lumberjacking after winning gobs of dosh on Rogue TV. Or you could sell it for an additional gob of dosh. Whatever; I don't judge."

  :info-apply "Instantly destroys an adjacent door."
  :gadget-effect (fn [self unid] (block

    (setv d (if unid (choice Pos.DIR8) (or (input-direction) (ret))))
    (setv p (+ G.player.pos d))
    (setv t (mget p))

    (.use-time-and-charge self)
    (if (instance? Door t)
      (do
        (msg "Bzzt! The door is no more." t t)
        (mset p (Floor)))
      (msg "{:Your} proves ineffective against {:the}." self t)))))

(def-itemtype Gadget "bee-machine" :name "personal beekeeping device"
  :price 30
  :info-flavor "A beekeeper is you! It's stuffed with everything you need to make your own honey. Including the bees. <b>Especially</b> the bees."
    ; "A foobar is you" is originally from the NES game Pro
    ; Wrestling, but is used here in a way paying tribute to the
    ; Kingdom of Loathing.
  :bee-summoning-range 3
  :bees-to-summon 10

  :info-apply "Creates {bees_to_summon} bees within {bee_summoning_range} squares of you."
  :gadget-effect (fn [self unid]

    (.use-time-and-charge self)
    (setv summoned 0)
    (for [p (shuffle (disc-taxi G.player.pos self.bee-summoning-range))]
      (when (room-for? Creature p)
        (rtv creature.monster.Bee p)
        (+= summoned 1)
        (when (> summoned self.bees-to-summon)
          (break))))
    (msg "A swarm of bees pours out of the device!")
    (cond
      [(= summoned 0)
        (msg "Then they all go right back in.")]
      [(< summoned (// self.bees-to-summon 2))
        (msg "Most of them go back in.")])))

(def-itemtype Gadget "gps" :name "GPS device"
  :price 20
  :info-flavor "They say it's unwise to use a GPS device as your only means of navigation in an unfamiliar area, but it's not as if you have lots of better options in a dungeon."
  :gps-range 10

  :info-apply "Reveals the map in a radius of {gps_range} squares around you."
  :gadget-effect (fn [self unid]

    (.use-time-and-charge self)
    (for [p (disc-taxi G.player.pos self.gps-range)]
      (setv (get G.seen-map p.x p.y) True))
    (soil-fov)
    (msg "{:The} reveals part of the dungeon around you." self)))

(assert (>= (len appearances)
  (len (filt (instance? Gadget it) (.values G.itypes)))))
