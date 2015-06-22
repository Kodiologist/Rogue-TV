#!/usr/bin/env hy

(import roguetv.cmdline)
(setv p (roguetv.cmdline.parse-args))

(import roguetv.main)

(roguetv.main.new-game p)

(import os.path)
(when (or (os.path.exists "roguetv_init.hy") (os.path.exists "roguetv_init.py"))
  (import roguetv-init))

(roguetv.main.main-loop)
