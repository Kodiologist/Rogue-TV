; Get information about the probabilitiy distribution of the
; number of obstacles, items, and benefits generated per level.
; The output is tables of quantiles per dungeon level/

(require kodhy.macros)

(import
  roguetv.mapgen
  [pandas :as pd])

(setv n 100000)
(setv qs [.01 .1 .2 .3 .4 .5 .6 .7 .8 .9 .99])

(for [x ["obstacles" "items" "benefits"]]
  (print x)
  (setv d (pd.DataFrame (rmap [dl [0 9 19]]
    (setv ps (.copy (get roguetv.mapgen.gen-count-params x)))
    (setv (get ps "dl") dl)
    (setv v (pd.Series (replicate n
      (apply roguetv.mapgen.gen-count [] ps))))
    (setv v (.quantile v qs))
    (setv v.name (inc dl))
    v)))
  (setv d.columns qs)
  (print d))
