(require kodhy.macros roguetv.macros)

(import
  os
  curses
  [heidegger.pos [Pos]]
  [roguetv.strings :as strings]
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

  (setv G.player (kwc Player
    :name "Josephine" :gender :female))
      ; Prefer :female for testing so it's easier to spot cases
      ; where I mistakenly wrote, e.g., "he" instead of "{p:he}".

  (setv G.dungeon-level 1)
  (reset-level))

(defn main-loop []

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
      (full-redraw)

      (setv old-clock-debt G.player.clock-debt-ms)
      (when (= (do-normal-command (get-normal-command)) :quit-game)
        (break))
      (setv G.last-action-duration (/
        (- G.player.clock-debt-ms old-clock-debt)
        Creature.clock-factor))

      (when (>= G.player.clock-debt-ms Creature.clock-factor)
        (setv skip (int (/ G.player.clock-debt-ms Creature.clock-factor)))
        (+= G.current-time skip)
        (-= G.player.clock-debt-ms (* skip Creature.clock-factor))
        (when (and G.time-limit (>= G.current-time G.time-limit))
          (msg :tara "Alas! {p.name} has run out of time.")
          (msg :bob (pick strings.bob-too-bad))
          (setv G.time-limit None)
          (setv G.endgame :out-of-time)))

      (when G.endgame
        (msg "Game over. Press Escape to quit.")
        (full-redraw)
        (hit-key-to-continue [G.key-escape])
        (break))))))
