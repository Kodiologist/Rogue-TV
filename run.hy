#!/usr/bin/env hy

(import roguetv.main)

(roguetv.main.new-game)

(import os.path)
(when (or (os.path.exists "roguetv_init.hy") (os.path.exists "roguetv_init.py"))
  (import roguetv-init))

(roguetv.main.main-loop)
