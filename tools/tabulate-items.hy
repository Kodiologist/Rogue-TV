(require kodhy.macros)

(import
  sys
  re
  subprocess
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
  (for [item (kwc sorted items :key (λ (, (- (it.generation-weight 0)) (- (or it.level-hi 9999)) it.price it.tid)))]
    (setv s (.format "{:20} {:3} {:.3} {:2} {:2} {:5.5} {}"
      item.tid
      item.price
      (keyword->str item.rarity)
      (if (= item.level-lo 0) "" (+ 1 item.level-lo))
      (if (none? item.level-hi) "" (+ 1 item.level-hi))
      (re.sub r"\Adark-" "D" (keyword->str item.color-fg))
      item.name.stem))
    (print (slice s 0 (int (subprocess.check-output ["tput" "cols"])))))
  (print))

(cond
  [(= mode "all")
    (show-items "All items" (λ True))]
  [(= mode "category") (do
    (setv cs [Soda Gadget Clothing Burden])
    (for [c cs]
      (show-items c.__name__ (λ (issubclass it c))))
    (show-items "Other" (fn [x] (not (afind-or (issubclass x it) cs)))))])
