(require kodhy.macros)

(import
  [roguetv.globals :as G]
  [roguetv.english [NounPhrase]]
  [roguetv.item.generic [Item def-itemtype]])

(def-itemtype Item "aoy"
  :name (kwc NounPhrase "Amulet of Yendor" :+the-proper)
  :char "☥"
  :color-bg :yellow
  :info-flavor "Actually, it's only a cheap plastic imitation of the Amulet of Yendor. In other words, it's a prop. The real Amulet has been lost to the ages. But this prop is worth a <b>fabulous</b> cash prize!"
  :info-carry "Allows you to take the final down elevator. In fact, the rules of Rogue TV stipulate that you're only granted the Amulet's <b>fabulous</b> cash prize if you take that down elevator. So, try not to run out of time."
  :price-grade (+ G.max-dungeon-level 10)
  :unique True
  :rarity :nongen
  :indestructible True)

(def-itemtype Item "test-item"
  :name "test item" :name-suffix (fn [self] "(testy)")
  :char "&"
  :info-flavor "This is a test item. It doesn't do anything."
  :price 11
  :rarity :nongen)
