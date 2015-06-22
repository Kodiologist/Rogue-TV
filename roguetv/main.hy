(require kodhy.macros roguetv.macros)

(import
  os
  [random [choice]]
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
  [roguetv.item [Item def-itemtype]]
  [roguetv.creature [Creature]]
  roguetv.item.gadget
  [roguetv.mapgen [reset-level]]
  [roguetv.display [full-redraw default-color describe-tile]]
  [roguetv.creature.player [Player]])

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

    (setv G.screen-mode :normal)

    (msg :tara "The game begins on a level with {} by {} squares. Good luck, {p}."
      G.map-width G.map-height)
    (describe-tile G.player.pos)

    (block :game-loop (while True

      (for [cr Creature.extant]
        (while (< cr.clock-debt-ms Creature.clock-factor)
          (cr.act)
          (when G.endgame
            (retf :game-loop))))
      (+= G.current-time 1)
      (for [cr Creature.extant]
        (-= cr.clock-debt-ms Creature.clock-factor)
        (assert (>= cr.clock-debt-ms 0)))

      (when (and G.time-limit (>= G.current-time G.time-limit))
        (msg :tara "Alas! {p:The} is out of time. {p:He} may keep only half {p:his} winnings.")
        (msg :bob (choice strings.bob-too-bad))
        (setv G.time-limit None)
        (setv G.endgame :out-of-time)
        (break))))

    (when G.endgame
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
      (hit-key-to-continue [G.key-escape]))))))
