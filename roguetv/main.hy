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
  [roguetv.input [hit-key-to-continue text-screen]]
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
  (setv Player.name p.name)

  (setv G.dungeon-level 0)
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
      (unless G.debug
        (text-screen (.format "\nIn the far-distant future of the year 200X, the story of the lone hero who retrieved the mystical Amulet of Yendor from the depths of the Dungeon of Doom has passed into legend. But the legend lives on in <b>Rogue TV</b>, the hit new game show where one brave contestant races against the clock to collect fabulous prizes! Host {} and color commentator {} will be your guides as you navigate the perils of a family-friendly state-of-the-art dungeon (designed anew for every contestant, and constructed to the highest standards of safety) to the cheers of adoring fans. Will you reach dungeon level {}, where the Amulet of Yendor lies? Or will you go home with a booby prize? It's up to you and the roll of the dice!"
          (color-xml "Tara Tanner" (get G.announcer-colors :tara))
          (color-xml "Bob McRobertson" (get G.announcer-colors :bob))
          (inc G.max-dungeon-level))))
      (msg :tara "The game begins on a level with {} by {} squares. Good luck, {p}."
        G.map-width G.map-height)
      (describe-tile G.player.pos))

    (block :game-loop (while True

      (while True
        (setv somebody-moved False)
        (for [cr (list Creature.extant)] (block :cr
          (while (< cr.clock-debt-ms Creature.clock-factor)
            (unless (in cr Creature.extant)
              ; We have to check again that this creature is around
              ; in case it disappeared (particularly, if the player
              ; went to a new level) since we started the whole loop.
              (retf :cr))
            (.act cr)
            (setv somebody-moved True)
            (when G.endgame
              (retf :game-loop)))))
        ; Re-loop through the creatures if anybody moved, in case
        ; some (.act cr) has caused a new creature to be created.
        (unless somebody-moved
          (break)))
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
    (setv winnings (filt
      (or (= G.endgame :won) (not (instance? (get G.itypes "aoy") it)))
      G.inventory))
    (defn total [l]
      (sum (amap it.price l)))
    (setv gross (total winnings))
    (when (in G.endgame [:out-of-time :resigned])
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
    (print "Saved game to" G.save-file-path))

  exit-reason)
