#!/usr/bin/env hy

;(import sys) (setv sys.path ["." "/usr/lib/python2.7" "/usr/lib/python2.7/lib-dynload"])

(require [kodhy.macros [afind-or qw]])

(import
  os
  os.path
  [roguetv.globals :as G]
  roguetv.cmdline
  roguetv.main)

(roguetv.cmdline.parse-env)
(setv p (roguetv.cmdline.parse-args))

(setv G.save-file-path p.save)
(setv G.scores-file-path p.scores)
(setv G.autosave (not p.no-autosave))
(setv G.debug p.debug)

(when (or p.show-scores p.show-all-scores)
  (roguetv.main.main-loop (if p.show-all-scores :show-all-scores :show-scores))
  (exit))

(if (os.path.exists p.save)
  (do
    (import roguetv.saves)
    (roguetv.saves.load-from-save-file G.save-file-path))
  (do
    (roguetv.main.new-game p)))

(when (afind-or (os.path.exists (+ "roguetv_init." it)) (qw hy py pyc))
  (import roguetv-init))

(setv exit-reason (roguetv.main.main-loop))

(unless (or p.debug (= exit-reason :save-and-quit))
  (try (os.remove p.save)
    (except [OSError])))
