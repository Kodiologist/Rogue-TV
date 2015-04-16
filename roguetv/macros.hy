(require kodhy.macros)

(import
  [hy [HySymbol]])

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

(defmacro set-self-nn [&rest props]
  `(do ~@(amap `(unless (none? ~it) (setv (. self ~it) ~it))  props)))

(defmacro rtv [f &rest args]
; (rtv m1.m2.f a b) =>
;   (do (import roguetv.m1.m2) (roguetv.m1.m2.f a b))
; but using a gensym for the module name.
  (setv i (.rindex f "."))
  (setv fname (HySymbol (slice f (+ 1 i))))
  (setv module (HySymbol (+ "roguetv." (slice f 0 i))))
  (setv g (gensym))
  `(do (import [~module :as ~g]) ((. ~g ~fname) ~@args)))
