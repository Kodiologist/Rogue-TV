(require kodhy.macros roguetv.macros)

(import
  [random [choice]]
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(setv cancel-keys [" " "\n" G.key-escape])

(defn y-or-n [prompt &optional [require-uppercase False]] (block
  (msg "{} {}" prompt
    (if require-uppercase "(Y/N; case-sensitive)" "(y/n)"));
  (rtv display.full-redraw)
  (setv G.last-new-message-number (dec (len G.message-log)))
  (while True
    (setv key (G.T.getkey))
    (unless require-uppercase
      (setv key (.upper key)))
    (when (= key "Y")
      (ret True))
    (when (= key "N")
      (ret False)))))

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
  (if (and (not pure) (.has-effect G.player (rtv-get creature.Confusion))
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

(defn get-normal-command [] (block
  (while True
    (setv key (G.T.getkey))
    (setv inp (cond

      [(= key "S")
        :save-and-quit]
      [(= key "Q")
        :resign-game]

      [(in key direction-keys)
        [:move (get-direction key)]]
      [(in key ["." "5" "KEY_B2"])
        [:wait]]

      [(= key ";")
        :look-mode]

      [(= key ":")
        :examine-ground]
      [(= key "t")
        :use-tile]

      [(= key "i")
        :inventory]
      [(= key ",")
        :pick-up]
      [(= key "d")
        :drop]
      [(= key "a")
        :apply-item]

      [(= key "W")
        :make-wall]
      [(= key "R")
        :reset-level]))

    (when inp
      (ret inp)))))

(defn text-screen [text]

  (rtv display.draw-text-screen text)
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
  :item "o"
  :tile "t"})

(defn look-mode [initial-pos]
  (setv prev-screen-mode G.screen-mode)
  (setv G.screen-mode :look)
  (setv focus G.player.pos)
  (while True
    (rtv display.full-redraw focus)
    (setv key (G.T.getkey))
    (cond

      [(in key cancel-keys)
        (break)]

      [(in key direction-keys) (do
        (setv new-focus (+ focus (kwc get-direction key :+pure)))
        (unless (rtv map.on-map new-focus)
          (continue))
        (setv focus new-focus))]

      [(in key (.values look-at-keys)) (do
        (setv mapobject-class (cond
          [(= (get look-at-keys :creature) key)
            (rtv-get creature.Creature)]
          [(= (get look-at-keys :item) key)
            (rtv-get item.Item)]
          [(= (get look-at-keys :tile) key)
            (rtv-get map.Tile)]))
        (when (and (mapobject-class.at focus)
            (get G.seen-map focus.x focus.y))
          (text-screen (.information (mapobject-class.at focus)))))]))

  (setv G.screen-mode prev-screen-mode))
