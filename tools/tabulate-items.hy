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

(defn colorize256 [fg bg text]
  (when (none? fg)
    (setv fg G.fg-color))
  (when (none? bg)
    (setv bg (G.pick-bg-color fg)))
  (.format "\x1b[38;5;{};48;5;{}m{}\x1b[0m"
    (get G.color-numbers fg) (get G.color-numbers bg) text))

(setv termcols (int (subprocess.check-output ["tput" "cols"])))

(defn show-items [title predicate]
  (setv items (list (filter predicate (.values G.itypes))))
  (print title (len items))
  (print "--------------------------------------------------")
  (for [item (kwc sorted items :key (λ (, (- (it.generation-weight 0)) (- (or it.level-hi 9999)) it.price it.tid)))]
    (setv s (.format "{:20} {:3} {:.3} {:2} {:2} � {}"
      item.tid
      item.price
      (keyword->str item.rarity)
      (if (= item.level-lo 0) "" (+ 1 item.level-lo))
      (if (none? item.level-hi) "" (+ 1 item.level-hi))
      item.name.stem))
    (setv s (slice s 0 termcols))
    (setv s (.replace s "�"
      (colorize256 item.color-fg item.color-bg item.char)))
    (print s))
  (print))

(cond
  [(= mode "all")
    (show-items "All items" (λ True))]
  [(= mode "category") (do
    (setv cs [Soda Gadget Clothing Burden])
    (for [c cs]
      (show-items c.__name__ (λ (issubclass it c))))
    (show-items "Other" (fn [x] (not (afind-or (issubclass x it) cs)))))])
