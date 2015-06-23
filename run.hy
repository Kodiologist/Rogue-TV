#!/usr/bin/env hy

(import
  os
  os.path
  roguetv.cmdline
  [roguetv.globals :as G]
  roguetv.main)

(setv p (roguetv.cmdline.parse-args))

(setv G.save-file-path p.save)
(setv G.debug p.debug)

(if (os.path.exists p.save)
  (do
    (import roguetv.saves)
    (roguetv.saves.load-from-save-file G.save-file-path))
  (do
    (roguetv.main.new-game p)))

(when (or (os.path.exists "roguetv_init.hy") (os.path.exists "roguetv_init.py"))
  (import roguetv-init))

(unless p.debug
  (try (os.remove p.save)
    (catch [_ OSError])))

(roguetv.main.main-loop)
