; This file is for special items that have negative effects and
; high values.

(require kodhy.macros)

(import
  random
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [disc-taxi Tile Floor Ice mset]]
  [roguetv.item.generic [Item def-itemtype item-pos]])

(defcls Burden [Item]
  rarity :rare
  price-adj :burden)

(defcls BigMoney [Burden]
  char "$"

  info-flavor "Big money! Really heavy money, in fact."
  info-carry "Slows your walking speed to {carry_speed_factor} times normal.")

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
  :superheavy True)

(defcls CursedGem [Burden]
  char "*"
  unique True)

(def-itemtype CursedGem "cursedgem-ice"
  :name (kwc NounPhrase "White Ice" :+the-proper)
  :color-fg :white
  :level-lo 4

  :ice-radius 3
  :ice-per-second 3

  :info-flavor "A frost-covered diamond the size of a baseball. You shiver just looking at it."
  :info-constant "Produces ice around itself. An ice tile is generated within {ice_radius} squares about {ice_per_second} times per second."

  :__init__ (meth [&kwargs kw]
    (apply CursedGem.__init__ [@] kw)
    (@schedule)
    None)

  :act (meth []
    (setv p (random.choice (disc-taxi (item-pos @) @ice-radius)))
    (when (instance? Floor (Tile.at p))
      (mset p (Ice)))
    (@take-time (long (randexp (/ 1 (/ @ice-per-second G.clock-factor)))))))
