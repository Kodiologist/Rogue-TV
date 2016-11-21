; Computes the proportion of chests that are nonempty on each level.

(import
  [roguetv.globals :as G]
  [roguetv.mapgen [*]])

(setv reps 1000)

(setv full-chests 0)
(setv empty-chests 0)

(for [dl (range (inc G.max-dungeon-level))]
  (for [_ (range reps)]
    (for [[in-chest? _] (select-items dl)]
      (when in-chest?
        (+= full-chests 1)))
    (for [o (select-obstacles dl)]
      (when (is o O-EmptyChest)
        (+= empty-chests 1))))
  (print (.format "{:2d} {:.2f}"
    (inc dl)
    (/ full-chests (+ full-chests empty-chests)))))
