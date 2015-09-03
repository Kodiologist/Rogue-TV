; This file is for special items that have negative effects and
; high values.

(require kodhy.macros)

(import
  [math [*]]
  [random [randrange choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret retf]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.item.generic [Item def-itemtype]])

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