(require kodhy.macros)

(import
  sys
  [random [randint]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  roguetv.item
    ; To ensure G.itypes is filled.
  [roguetv.mapgen [Obstacle]])

(setv [_ mode dl] sys.argv)
(setv dl (int dl))

(for [round [0 1]]
  (setv l (kwc sorted :+reverse
    (amap (, (it.generation-weight dl) it)
    (filt (!= it.rarity :nongen)
    (get [Obstacle.types (.values G.itypes)] round)))))
  (cond
    [(= mode "probs") (do
      (setv total (sum (map first l)))
      (for [[w c] l]
        (print (.format "{:1.03f} {.__name__}" (/ w total) c))))]
    [(= mode "sample") (do
      (setv n (get
        [
          (randint (+ dl 3) (* 2 (+ dl 3)))
          (inc (randpois (+ 3 (/ dl 5))))]
        round))
      (for [x (sorted (replicate n (weighted-choice l)))]
        (print x.__name__)))])
  (print))
