(require kodhy.macros)

(import
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [randpop]]
  [roguetv.item.generic [Item ItemAppearance def-itemtype]])

(defclass Gadget [Item] [
  [max-charges 10]

  [__init__ (fn [self &optional charges &kwargs rest]
    (apply Item.__init__ [self] rest)
    (setv self.charges (if (none? charges) self.max-charges charges))
    None)]])

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

(defn def-gadget [tid name]
  (def-itemtype Gadget tid name
    :char "/"))

(defn randomize-appearances []
  (setv unused-appearances (.values appearances))
  (for [itype (filt (issubclass it Gadget) (.values G.itypes))]
    (.set-appearance itype (randpop unused-appearances))))

(def-gadget "hookshot" (NounPhrase "hookshot"))
(def-gadget "panic-button" (NounPhrase "panic button"))
(def-gadget "chainsaw" (NounPhrase "chainsaw"))
