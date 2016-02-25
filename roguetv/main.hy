(require kodhy.macros roguetv.macros)

(import
  os
  locale
  curses
  [itertools [combinations]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret concat]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [hit-key-to-continue text-screen]]
  [roguetv.types [Scheduled]]
  [roguetv.map [room-for?]]
  [roguetv.item [Item ItemAppearance def-itemtype]]
  [roguetv.mapgen [reset-level]]
  [roguetv.attrstr [default-color]]
  [roguetv.display [full-redraw describe-tile]]
  [roguetv.creature.player [Player]]
  [roguetv.saves [write-save-file]]
  [roguetv.scores [add-current-game-to-scores show-scores]])

(defn new-game [parsed-cmdline-args]
  (setv p parsed-cmdline-args)

  (setv G.player (Player))
  (setv Player.name p.name)

  (setv G.seeds {"map" p.map-seed "general" p.general-seed})
  (print (.format "Using map seed {}, general seed {}"
    (get G.seeds "map") (get G.seeds "general")))
  (random.seed (get G.seeds "map"))
  (ItemAppearance.randomize-appearances)
  (random.seed (get G.seeds "general"))

  (setv G.dungeon-level (or p.start-at-dl 0))
  (reset-level)

  (setv (get G.dates "started") (real-timestamp)))

(defn main-loop [&optional special]

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

    (when special
      (kwc show-scores G.scores-file-path :show-all (ecase special
        [:show-scores False]
        [:show-all-scores True]))
      (ret :just-showing-scores))

    (unless (get G.dates "loaded")
      (unless G.debug
        (text-screen (.format "\nIn the far-distant future of the year 200X, the story of the lone hero who retrieved the mystical Amulet of Yendor from the depths of the Dungeon of Doom has passed into legend. But the legend lives on in <b>Rogue TV</b>, the hit new game show where one brave contestant races against the clock to collect fabulous prizes! Host {} and color commentator {} will be your guides as you navigate the perils of a family-friendly state-of-the-art dungeon (designed anew for every contestant, and constructed to the highest standards of safety) to the cheers of adoring fans. Will you reach <b>dungeon level {}</b>, where the Amulet of Yendor (worth a fabulous <b>${}</b>) lies? Or will you go home with a booby prize? It's up to you and the roll of the dice!"
          (color-xml "Tara Tanner" (get G.announcer-colors :tara))
          (color-xml "Robert Babaghanoush" (get G.announcer-colors :bob))
          (inc G.max-dungeon-level)
          (. (get G.itypes "aoy") price))))
      (msg :tara "The game begins on a level with {} by {} squares. Good luck, {p}."
        G.map-width G.map-height)
      (describe-tile G.player.pos))

    (Scheduled.game-loop)

    (assert G.endgame)
    (setv (get G.dates "ended") (real-timestamp))
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
      (setv winnings (list
        (kwc max :key total
        (filt (<= (total it) (/ gross 2))
        (concat
        (amap (list (combinations winnings it))
        (range (inc (len winnings)))))))))
      (setv gross (total winnings)))
    (kwc .sort winnings :key (λ (, (- it.price) it.tid)))
    (unless G.debug
      (add-current-game-to-scores G.scores-file-path winnings gross))
    (msg "Game over. Your total winnings are ${}. Hit \"!\" to quit." gross)
    (full-redraw)
    (hit-key-to-continue "!")
    (unless G.debug
      (show-scores G.scores-file-path))
    :game-over))))

  (when (= exit-reason :save-and-quit)
    (print "Saving...")
    (write-save-file G.save-file-path)
    (print "Saved game to" G.save-file-path))

  exit-reason)
