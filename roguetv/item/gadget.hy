(require kodhy.macros)

(import
  [random [randrange]]
  [heidegger.pos [Pos]]
  [kodhy.util [cat ret retf]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [input-direction]]
  [roguetv.map [Tile Floor Door room-for? mset ray-taxicab disc-taxicab]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

(defclass Gadget [Item] [
  [max-charges 10]
  [apply-time 1]
  [char "/"]

  [__init__ (fn [self &optional charges &kwargs rest]
    (apply Item.__init__ [self] rest)
    (setv self.charges (if (none? charges) self.max-charges charges))
    None)]

  [display-name (fn [self]
    (cat
      (.display-name (super Gadget self))
      (and self.appearance.known
        (.format " ({})" self.charges))))]

  [applied (fn [self cr] (block
    ; Do we have a charge to spare?
    (when (= self.charges 0)
      (msgp cr :bob "That oojah's all chatty, kemosabe.")
      (ret))
    ; Identify the item type.
    (unless self.appearance.known
      (setv self.appearance.known True)
      (msg "You have:  {}" (self.invstr)))
    ; Now you get the gadget effect.
    (self.gadget-effect cr)))]

  [gadget-effect (fn [self cr]
    ; Do whatever the gadget should do. If the user ends up
    ; really getting the gadget effect (e.g., they don't cancel out
    ; of a direction prompt), be sure to call .use-time-and-charge
    ; before otherwise affecting the game world.
    (.use-time-and-charge self cr)
    (msg "Nothing happens."))]

  [use-time-and-charge (fn [self cr]
    ; Use up a charge and some time.
    (cr.take-time self.apply-time)
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
  :teleport-tries 100
  :gadget-effect (fn [self cr] (block :gadget

    ; Find a place to teleport to.
    (block
      (for [_ (range self.teleport-tries)]
        (setv p-to (Pos (randrange G.map-width) (randrange G.map-height)))
        (when (and (room-for? (type cr) p-to) (instance? Floor (Tile.at p-to)))
          (ret)))
      ; We failed to find a legal square.
      (.use-time-and-charge self cr)
      (msgp cr "You feel cramped.")
      (retf :gadget))

    ; Now teleport there.
    (.use-time-and-charge self cr)
    (.move cr p-to)
    (when (player? cr)
      (msg :tara "{p:He's} teleported to another part of the level.")))))

(def-itemtype Gadget "hookshot"
  :hookshot-dist 8
  :hookshot-travel-speed 2
  :gadget-effect (fn [self cr] (block :gadget

    (setv d (or (input-direction) (ret)))

    ; Find our destination square.
    (setv ahead (+ cr.pos d))
    (setv path (ray-taxicab ahead d self.hookshot-dist))
    (block
      (for [p path]
        (when (. (Tile.at p) blocks-movement)
          (ret))
        (whenn (.at cr p)
          (.use-time-and-charge self cr)
          (msgp cr :tara "{p:name}'s {} bounces off {}."
            self it)
          (retf :gadget)))
      (msg "Your {} can only reach objects up to {} squares away."
        self self.hookshot-dist)
      (retf :gadget))
    (when (= p ahead)
      (msg :tara "It looks like {p:name}'s {} isn't very useful at that range."
        self)
      (retf :gadget))
    (setv p-to (- p d))

    ; And away we go.
    (.use-time-and-charge self cr)
    (.take-time cr (/ (len-taxicab (- p-to cr.pos)) self.hookshot-travel-speed))
    (.move cr p-to)
    (msg "{:The} pulls you ahead." self))))

(def-itemtype Gadget "chainsaw"
  :gadget-effect (fn [self cr] (block

    (setv d (or (input-direction) (ret)))
    (setv p (+ cr.pos d))
    (setv t (Tile.at p))

    (if (instance? Door t)
      (do
        (.use-time-and-charge self cr)
        (msgp cr "Bzzt! The door is no more.")
        (mset p (Floor)))
      (msgp cr "Your {} won't help with that." self)))))

(def-itemtype Gadget "GPS device"
  :gps-range 10
  :gadget-effect (fn [self cr]

    (when (player? cr)
      (.use-time-and-charge self cr)
      (for [p (disc-taxicab cr.pos self.gps-range)]
        (setv (get G.seen-map p.x p.y) True))
      (soil-fov)
      (msg "{:The} reveals part of the dungeon around you." self))))
