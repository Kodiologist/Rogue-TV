#!/usr/bin/env hy

;(import sys) (setv sys.path ["." "/usr/lib/python2.7" "/usr/lib/python2.7/lib-dynload"])

(import
  os
  os.path
  roguetv.cmdline
  [roguetv.globals :as G]
  roguetv.main)

(setv p (roguetv.cmdline.parse-args))

(setv G.save-file-path p.save)
(setv G.autosave (not p.no-autosave))
(setv G.debug p.debug)

(if (os.path.exists p.save)
  (do
    (import roguetv.saves)
    (roguetv.saves.load-from-save-file G.save-file-path))
  (do
    (roguetv.main.new-game p)))

(when (or (os.path.exists "roguetv_init.hy") (os.path.exists "roguetv_init.py"))
  (import roguetv-init))

(setv exit-reason (roguetv.main.main-loop))

(unless (or p.debug (= exit-reason :save-and-quit))
  (try (os.remove p.save)
    (catch [_ OSError])))
