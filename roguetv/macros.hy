(require [kodhy.macros [amap]])

(import
  [hy [HySymbol HyString]])

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

(defmacro set-self-nn [&rest props]
  `(do ~@(amap `(unless (none? ~it) (setv (. self ~it) ~it))  props)))

(defmacro rtv-get [x]
  (setv i (.rindex x "."))
  (setv xname (HySymbol (cut x (+ 1 i))))
  (setv module (HySymbol (+ "roguetv." (cut x 0 i))))
  (setv g (gensym))
  `(do (import [~module :as ~g]) (. ~g ~xname)))

(defmacro rtv [f &rest args]
; (rtv m1.m2.f a b) =>
;   (do (import roguetv.m1.m2) (roguetv.m1.m2.f a b))
; but using a gensym for the module name.
  `((rtv-get ~f) ~@args))
