; This file is for special items that have negative effects and
; high values.

(require [kodhy.macros [meth]])

(import
  random
  [kodhy.util [T F]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [disc-taxi Tile Floor Ice mset]]
  [roguetv.item.generic [Item def-itemtype item-pos]])

(defclass Burden [Item] [
  rarity :rare
  price-adj :burden])

(defclass BigMoney [Burden] [
  char "$"

  info-flavor "Big money! Really heavy money, in fact."
  info-carry "Slows your walking speed to {carry-speed-factor} times normal."])

(def-itemtype BigMoney "briefcase-cash" :name "briefcase full of cash"
  :color-fg :dark-green
  :level-lo 4
  :carry-speed-factor .9)

(def-itemtype BigMoney "briefcase-silver" :name "briefcase full of silver ingots"
  :color-fg :dark-gray
  :level-lo 9
  :carry-speed-factor .75)

(def-itemtype BigMoney "briefcase-gold" :name "briefcase full of gold ingots"
  :color-bg :gold
  :level-lo 14
  :carry-speed-factor .5)

(def-itemtype BigMoney "gold-boulder" :name "solid gold boulder"
  :char "0"
  :color-bg :gold
  :level-lo 19

  :info-carry "You can't walk. At all."
  :superheavy T)

(defclass CursedGem [Burden] [
  char "*"
  unique T])

(def-itemtype CursedGem "cursedgem-ice"
  :name (NounPhrase "White Ice" :the-proper T)
  :color-fg :white
  :level-lo 4

  :ice-radius 3
  :ice-per-second 3

  :info-flavor "A frost-covered diamond the size of a baseball. You shiver just looking at it."
  :info-constant "Produces ice around itself. An ice tile is generated within {ice-radius} squares about {ice-per-second} times per second."

  :__init__ (meth [&kwargs kw]
    (CursedGem.__init__ @@ #** kw)
    (@schedule)
    None)

  :act (meth []
    (setv p (random.choice (disc-taxi (item-pos @@) @ice-radius)))
    (when (instance? Floor (Tile.at p))
      (mset p (Ice)))
    (@take-time (int (randexp (/ 1 (/ @ice-per-second G.clock-factor)))))))
