(require kodhy.macros)

(import
  sys
  [itertools [groupby]]
  [random [randint]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  roguetv.item
    ; To ensure G.itypes is filled.
  [roguetv.mapgen [Obstacle select-items select-obstacles select-benefits]]
  [kodhy.util [shift weighted-choice]])

(shift sys.argv)
(setv mode (shift sys.argv))
(setv dl (shift sys.argv))
(setv chest? (and sys.argv (shift sys.argv)))

(setv dl (int dl))

(cond
  [(= mode "probs")
    (for [ition [0 1]]
      (setv l (kwc sorted :+reverse
        (amap (, (it.generation-weight dl chest?) it)
        (filt (!= it.rarity :nongen)
        (get [Obstacle.types (.values G.itypes)] ition)))))
      (setv total (sum (map first l)))
      (for [[w c] l]
        (print (.format "{:1.03f} {.__name__}" (/ w total) c)))
      (print))]
  [(= mode "sample") (do
    (for [[s f] [["Obstacles" select-obstacles] ["Benefits" select-benefits]]]
      (print "---" s "---")
      (for [[_ l] (groupby (sorted (f dl)))]
        (setv l (list l))
        (print (len l) "×" (. (first l) __name__)))
      (print))
    (print "--- Items ---")
    (for [[in-chest? itype] (sorted (select-items dl))]
      (print itype.tid (if in-chest? "(chest)" ""))))])
