(require kodhy.macros)

(import
  [random [randrange]]
  [heidegger.pos [Pos]]
  [kodhy.util [cat ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile Floor room-for? recompute-fov]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

(defclass Gadget [Item] [
  [max-charges 3]
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
    ; Use up a charge and some time.
    (when (= self.charges 0)
      (when (is cr G.player)
        (msg :bob "That oojah's all chatty, kemosabe."))
      (ret))
    (-= self.charges 1)
    (cr.take-time self.apply-time)
    ; Identify the item type.
    (unless self.appearance.known
      (setv self.appearance.known True)
      (msgn "You have:  {}" (self.invstr)))
    ; Now you get the gadget effect.
    (self.gadget-effect cr)))]

  [gadget-effect (fn [self cr]
    (msgn "Nothing happens."))]])

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
  :gadget-effect (fn [self cr]
    ; Find a place to teleport to.
    (while True
      (setv p-to (Pos (randrange G.map-width) (randrange G.map-height)))
      (when (and (room-for? (type cr) p-to) (instance? Floor (Tile.at p-to)))
        (break)))
    ; Now teleport there.
    (.move cr p-to)
    (when (is cr G.player)
      (recompute-fov)
      (msg :tara "{p:He's} teleported to another part of the level."))))

(def-itemtype Gadget "hookshot")
(def-itemtype Gadget "chainsaw")
