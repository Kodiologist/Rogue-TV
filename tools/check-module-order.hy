; This script checks that the roguetv.* modules obey a standard
; order in which to import modules.
;
; While the order in which modules are listed in (import ...)
; isn't very important, the module load order potentially is, due
; to the perils of recursive imports. Specifying and enforcing a
; canonical load order (and import order) helps to clarify which
; modules are allowed to import from which others.

(require [kodhy.macros [lc amap filt]])
(import os.path)

(setv modules (with [o (open "module-order.txt")]
  (filt it (.split (.read o) "\n"))))

(for [module (filt (.startswith it "roguetv.") modules)]

  (setv import-form (do
    (setv fname (.replace (.replace module "." "/") "-" "_"))
    (when (and (os.path.exists fname) (os.path.isdir fname))
      (continue))
    (when (os.path.exists (+ fname ".py"))
      (continue))
    (+= fname ".hy")
    (setv form None)
    (with [o (open fname)]
      (try
        (while True
          (setv form (read o))
          (when (= (first form) 'import)
            (break)))
        (except [EOFError])))
    form))

  (unless import-form (continue))

  (setv imports-from
    (filt (in it modules)
    (amap (if (instance? list it) (first it) it)
    (cut import-form 1))))
  (setv ix (amap (.index modules it) imports-from))
  (setv sorted? (all (lc [[a b] (zip ix (rest ix))] (< a b))))
  (unless sorted?
    (print (.format "{} import order - WRONG: {}" module (list (zip imports-from ix)))))
  (setv backwards-deps? (any (amap (>= it (.index modules module)) ix)))
  (when backwards-deps?
    (print module "no backwards deps - WRONG")))
