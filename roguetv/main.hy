(require kodhy.macros roguetv.macros)

(import
  os
  curses
  [heidegger.pos [Pos]]
  [roguetv.strings :as strings]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [hit-key-to-continue get-normal-command]]
  [roguetv.map [room-for?]]
  [roguetv.item [Item def-itemtype]]
  roguetv.item.gadget
  [roguetv.creature [Creature Player]]
  [roguetv.mapgen [reset-level]]
  [roguetv.display [full-redraw default-color describe-tile]]
  [roguetv.actions [do-normal-command]])

(defn new-game []

  (roguetv.item.gadget.randomize-appearances)

  (setv G.player (Player))
  (setv Player.gender :female)
    ; Prefer :female for testing so it's easier to spot cases
    ; where I mistakenly wrote, e.g., "he" instead of "{p:he}".
  (setv Player.name (NounPhrase "contestant"))
          ;(kwc NounPhrase "Josephine" :+proper))

  (setv G.dungeon-level 1)
  (reset-level))

(defn main-loop [] (block :main-loop

  (unless (in "ESCDELAY" os.environ)
    (setv (get os.environ "ESCDELAY") "10"))
      ; This ensures curses will respond to the escape key quickly
      ; in keypad mode (which is enabled by curses.wrapper).

  (curses.wrapper (fn [scr]

    (setv G.T scr)
    (setv [G.screen-height G.screen-width] (G.T.getmaxyx))
    (curses.curs-set 0) ; Make the cursor invisible.
    (G.T.bkgd (ord " ") (default-color)) ; Set the background color.

    (describe-tile G.player.pos)

    (while True

      (for [cr Creature.extant]
        (while (< cr.clock-debt-ms Creature.clock-factor)
          (cr.act)))
      (+= G.current-time 1)
      (for [cr Creature.extant]
        (-= cr.clock-debt-ms Creature.clock-factor)
        (assert (>= cr.clock-debt-ms 0)))

      (when (and G.time-limit (>= G.current-time G.time-limit))
        (msg :tara "Alas! {p:The} has run out of time.")
        (msg :bob (pick strings.bob-too-bad))
        (setv G.time-limit None)
        (setv G.endgame :out-of-time))

      (when G.endgame
        (msg "Game over. Press Escape to quit.")
        (full-redraw)
        (hit-key-to-continue [G.key-escape])
        (break)))))))
