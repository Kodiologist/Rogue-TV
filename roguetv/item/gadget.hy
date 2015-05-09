(require kodhy.macros)

(import
  [random [randrange]]
  [heidegger.pos [Pos]]
  [kodhy.util [cat ret retf]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile Floor room-for? recompute-fov]]
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
      (when (is cr G.player)
        (msg :bob "That oojah's all chatty, kemosabe."))
      (ret))
    ; Identify the item type.
    (unless self.appearance.known
      (setv self.appearance.known True)
      (msgn "You have:  {}" (self.invstr)))
    ; Now you get the gadget effect.
    (unless (self.gadget-effect cr)
      (ret))
    ; Use up a charge and some time.
    (-= self.charges 1)
    (cr.take-time self.apply-time)))]

  [gadget-effect (fn [self cr]
    ; Return a boolean indicating whether the gadget was actually
    ; used.
    (msgn "Nothing happens.")
    True)]])

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
      (when (is cr G.player)
        (msgn "You feel cramped."))
      (retf :gadget True))
    ; Now teleport there.
    (.move cr p-to)
    (when (is cr G.player)
      (recompute-fov)
      (msg :tara "{p:He's} teleported to another part of the level."))
    True)))

(def-itemtype Gadget "hookshot"
  :hookshot-dist 4
  :hookshot-travel-speed 2
  :gadget-effect (fn [self cr] (block :gadget

    (setv d Pos.NORTH)

    ; Find our destination square.
    (setv ahead (+ cr.pos d))
    (setv path (ray ahead d self.hookshot-dist))
    (block
      (for [p path]
        (when (. (Tile.at p) blocks-movement)
          (ret))
        (whenn (.at cr p)
          (when (is cr G.player)
            (msg :tara "{p:name}'s {} bounces off {}."
              self it))
          (retf :gadget True)))
      (msgn "Your {} can only reach {} squares ahead."
        self self.hookshot-dist)
      (retf :gadget False))
    (when (= p ahead)
      (msg :tara "It looks like {p:name}'s {} isn't very useful at that range."
        self)
      (retf :gadget False))
    (setv p-to (- p d))

    ; And away we go.
    (.take-time cr (/ (len-taxicab (- p-to cr.pos)) self.hookshot-travel-speed))
    (.move cr p-to)
    (msgn "{:The} pulls you ahead." self)
    True)))

(def-itemtype Gadget "chainsaw")
