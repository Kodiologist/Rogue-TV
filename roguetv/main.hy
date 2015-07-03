(require kodhy.macros roguetv.macros)

(import
  os
  locale
  [random [choice]]
  [datetime [datetime]]
  curses
  [itertools [combinations]]
  [heidegger.pos [Pos]]
  [kodhy.util [retf concat]]
  [roguetv.strings :as strings]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [hit-key-to-continue]]
  [roguetv.map [room-for?]]
  [roguetv.item [Item ItemAppearance def-itemtype]]
  [roguetv.creature [Creature]]
  [roguetv.mapgen [reset-level]]
  [roguetv.attrstr [default-color]]
  [roguetv.display [full-redraw describe-tile]]
  [roguetv.creature.player [Player]]
  [roguetv.saves [write-save-file]])

(defn new-game [parsed-cmdline-args]
  (setv p parsed-cmdline-args)

  (ItemAppearance.randomize-appearances)

  (setv G.player (Player))
  (setv Player.gender p.gender)
  (setv Player.name p.name)

  (setv G.dungeon-level 1)
  (reset-level)

  (setv (get G.dates "started") (.isoformat (datetime.utcnow))))

(defn main-loop []

  (unless (in "ESCDELAY" os.environ)
    (setv (get os.environ "ESCDELAY") "10"))
      ; This ensures curses will respond to the escape key quickly
      ; in keypad mode (which is enabled by curses.wrapper).

  (locale.setlocale locale.LC-ALL "")
  (setv G.locale-encoding (locale.getpreferredencoding))

  (setv exit-reason (block :curses-wrapper (curses.wrapper (fn [scr]

    (setv G.T scr)
    (setv [G.screen-height G.screen-width] (G.T.getmaxyx))
    (curses.curs-set 0) ; Make the cursor invisible.
    (G.T.bkgd (ord " ") (default-color)) ; Set the background color.

    (setv G.screen-mode :normal)

    (unless (get G.dates "loaded")
      (msg :tara "The game begins on a level with {} by {} squares. Good luck, {p}."
        G.map-width G.map-height)
      (describe-tile G.player.pos))

    (block :game-loop (while True

      (for [cr Creature.extant] (block :cr
        (while (< cr.clock-debt-ms Creature.clock-factor)
          (unless (in cr Creature.extant)
            ; We have to check again that this creature is around
            ; in case it disappeared (particularly, if the player
            ; went to a new level) since we started the whole loop.
            (retf :cr))
          (cr.act)
          (when G.endgame
            (retf :game-loop)))))
      (+= G.current-time 1)
      (for [cr Creature.extant]
        (-= cr.clock-debt-ms Creature.clock-factor)
        (assert (>= cr.clock-debt-ms 0)))

      (when (and G.time-limit (>= G.current-time G.time-limit))
        (msg :tara "Alas! {p:The} is out of time. {p:He} may keep only half {p:his} winnings.")
        (msg :bob (choice strings.bob-too-bad))
        (setv G.time-limit None)
        (setv G.endgame :out-of-time)
        (retf :game-loop))))

    (assert G.endgame)
    (setv winnings G.inventory)
    (defn total [l]
      (sum (amap it.price l)))
    (setv gross (total winnings))
    (when (= G.endgame :out-of-time)
      ; Reduce the player's winnings to the combination of
      ; items with the highest total value less than or equal to
      ; half the original sum of values.
      ;
      ; Yes, we're brute-forcing the knapsack problem here.
      ; This should be fine so long as the inventory is small.
      (setv winnings
        (kwc max :key total
        (filt (<= (total it) (/ gross 2))
        (concat
        (amap (list (combinations winnings it))
        (range (inc (len winnings))))))))
      (setv gross (total winnings)))
    (msg "Game over. Your total winnings are ${}. Press Escape to quit." gross)
    (full-redraw)
    (hit-key-to-continue [G.key-escape])
    :game-over))))

  (when (= exit-reason :save-and-quit)
    (print "Saving...")
    (write-save-file G.save-file-path)
    (print "Saved game to" G.save-file-path)))
