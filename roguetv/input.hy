(require kodhy.macros roguetv.macros)

(import
  [random [choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(setv cancel-keys [" " "\n" G.key-escape])

(defn user-confirms []
  (msg "(Hit \"!\" to confirm or any other key to cancel.)")
  (rtv display.full-redraw)
  (setv G.last-new-message-number (dec (len G.message-log)))
  (= (G.T.getkey) "!"))

(defn hit-key-to-continue [keys]
  (while True
    (when (in (G.T.getkey) keys)
      (break))))

(setv direction-keys {

  "KEY_UP" Pos.NORTH
  "8" Pos.NORTH
  "k" Pos.NORTH
  "KEY_DOWN" Pos.SOUTH
  "2" Pos.SOUTH
  "j" Pos.SOUTH
  "KEY_LEFT" Pos.WEST
  "4" Pos.WEST
  "h" Pos.WEST
  "KEY_RIGHT" Pos.EAST
  "6" Pos.EAST
  "l" Pos.EAST

  "KEY_HOME" Pos.NW
  "7" Pos.NW
  "y" Pos.NW
  "KEY_PPAGE" Pos.NE
  "9" Pos.NE
  "u" Pos.NE
  "KEY_END" Pos.SW
  "1" Pos.SW
  "b" Pos.SW
  "KEY_NPAGE" Pos.SE
  "3" Pos.SE
  "n" Pos.SE})

(defn get-direction [key &optional [pure False]]
  (setv d (get direction-keys key))
  (if (and (not pure) (.get-effect G.player (rtv-get creature.Confusion))
      (chance G.confusion-misdirect-prob))
    (choice (filt (!= it d) Pos.DIR8))
    d))

(defn input-direction [] (block
  (msg "In what direction?")
  (rtv display.full-redraw)
  (setv G.last-new-message-number (dec (len G.message-log)))
  (while True
    (setv key (G.T.getkey))
    (when (in key direction-keys)
      (ret (get-direction key)))
    (when (in key cancel-keys)
      (ret None)))))

(setv normal-command-keys [
  ["?" :help "Show this help screen"]
  ["S" :save-and-quit "Save your game and quit the program"]
  ["Q" :resign-game "Resign the game"]
  ["m" :message-log "See old messages"]
  [";" :look-mode "Enter look mode"]
  [":" :examine-ground "Name what's beneath you"]
  ["t" :use-tile "Use terrain (e.g., an elevator)"]
  ["i" :inventory "Show inventory and examine items"]
  ["," :pick-up "Pick up an item at your feet"]
  ["d" :drop "Drop an item"]
  ["a" :apply-item "Apply (use) an item"]
  ["W" :make-wall :debug]
  ["R" :reset-level :debug]])

(defn get-normal-command [] (block
  (while True
    (setv key (G.T.getkey))
    (setv inp (cond
      [(in key direction-keys)
        [:move (get-direction key)]]
      [(in key ["." "5" "KEY_B2"])
        :wait]
      [(in key ["<" ">"])
        ; Muscle memory for using stairs.
        :use-tile]
      [(in key (map first normal-command-keys))
        (second (afind (= (first it) key) normal-command-keys))]))
    (when inp
      (ret inp)))))

(defn text-screen [text]

  (rtv display.draw-text-screen text)
  (G.T.refresh)

  (while True
    (when (in (G.T.getkey) cancel-keys)
      (break))))

(defn message-log-screen []

  (rtv display.draw-message-log True)
  (G.T.refresh)

  (while True
    (when (in (G.T.getkey) cancel-keys)
      (break))))

(defn inventory-loop [prompt &optional [select True]]

  (rtv display.draw-inventory prompt)
  (G.T.refresh)

  (setv il (amap it.invlet G.inventory))

  (while True
    (setv key (G.T.getkey))
    (setv inp (cond

      [(and select (in key G.invlets))
        (if (in key il)
          (.index il key)
          (do
            (msg "You don't have such an item.")
            :quit))]

      [(in key cancel-keys)
        :quit]))

    (when (not (none? inp))
      (break)))

  (when (and (numeric? inp) (< inp (len G.inventory)))
    inp))

(def look-at-keys {
  :creature "c"
  :item "i"
  :tile "t"})

(defn look-mode [initial-pos]
  (setv prev-screen-mode G.screen-mode)
  (setv G.screen-mode :look)
  (setv focus G.player.pos)
  (while True
    (rtv display.full-redraw focus)
    (setv key (G.T.getkey))
    (cond

      [(or (in key cancel-keys)
         (= key (first (afind (= (second it) :look-mode) normal-command-keys))))
        (break)]

      [(in key direction-keys) (do
        (setv new-focus (+ focus (kwc get-direction key :+pure)))
        (unless (rtv map.on-map new-focus)
          (continue))
        (setv focus new-focus))]

      [(in key (.values look-at-keys)) (do
        (setv mapobject-class (ecase key
          [(get look-at-keys :creature)
            (rtv-get creature.Creature)]
          [(get look-at-keys :item)
            (rtv-get item.Item)]
          [(get look-at-keys :tile)
            (rtv-get map.Tile)]))
        (when (and (mapobject-class.at focus)
            (get G.seen-map focus.x focus.y))
          (text-screen (.information (mapobject-class.at focus)))))]))

  (setv G.screen-mode prev-screen-mode))
