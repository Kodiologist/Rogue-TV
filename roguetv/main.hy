(require kodhy.macros)

(import
  os
  curses
  [heidegger.pos [Pos]]
  [roguetv.strings :as strings]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [hit-key-to-continue get-normal-command]]
  [roguetv.map [recompute-fov room-for?]]
  [roguetv.item [ItemType Item]]
  [roguetv.creature [Creature]]
  [roguetv.mapgen [reset-level]]
  [roguetv.display [full-redraw default-color describe-tile]]
  [roguetv.actions [do-normal-command]])

(setv G.player (kwc Creature
  :name "Josephine" :gender :female
    ; Prefer :female for testing so it's easier to spot cases
    ; where I mistakenly wrote, e.g., "he" instead of "{p:he}".
  :char "@" :color-bg :yellow))

(setv G.dungeon-level 1)
(reset-level)

(kwc ItemType
  :tid "toaster" :name "a toaster"
  :char ")" :color-fg :dark-green)
(kwc ItemType
  :tid "galoshes" :name "a pair of galoshes"
  :char "[" :color-fg :brown)
(kwc ItemType
  :tid "ottoman" :name "an ottoman"
  :char "(" :color-fg :red)
(kwc ItemType
  :tid "food-processor" :name "a food processor"
  :char "!" :color-fg :dark-blue)

(setv starting-items 15)
(for [x (range -2 3)]
  (for [y (range -2 3)]
    (setv p (+ G.player.pos (Pos x y)))
    (when (room-for? Item p)
      (kwc Item :itype (pick (.values G.itypes)) :pos p)
      (-= starting-items 1)
      (when (zero? starting-items)
        (break)))))

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
      (msgn "Game over. Press Escape to quit.")
      (full-redraw)
      (hit-key-to-continue [G.key-escape])
      (break)))))
