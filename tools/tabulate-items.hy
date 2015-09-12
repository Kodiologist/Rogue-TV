(require kodhy.macros)

(import
  sys
  [kodhy.util [keyword->str]]
  [roguetv.globals :as G]
  roguetv.item
  [roguetv.item.gadget [Gadget]]
  [roguetv.item.soda [Soda]]
  [roguetv.item.clothing [Clothing]]
  [roguetv.item.burden [Burden]])

(setv mode (second sys.argv))

(defn show-items [title predicate]
  (setv items (list (filter predicate (.values G.itypes))))
  (print title (len items))
  (print "--------------------------------------------------")
  (for [item (kwc sorted items :key (λ (, (- (it.generation-weight 0)) (- it.level-hi) it.price it.tid)))]
    (print (.format "{:20} {:3} {:.3} {:2} {:2} {}"
      item.tid item.price
      (keyword->str item.rarity)
      (if (= item.level-lo 0) "" (+ 1 item.level-lo))
      (if (= item.level-hi G.max-dungeon-level) "" (+ 1 item.level-hi))
      item.name.stem)))
  (print))

(cond
  [(= mode "all")
    (show-items "All items" (λ True))]
  [(= mode "category") (do
    (setv cs [Soda Gadget Clothing Burden])
    (for [c cs]
      (show-items c.__name__ (λ (issubclass it c))))
    (show-items "Other" (fn [x] (not (afind-or (issubclass x it) cs)))))])
