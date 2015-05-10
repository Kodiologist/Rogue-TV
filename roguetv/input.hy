(require kodhy.macros roguetv.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(setv cancel-keys [" " "\n" G.key-escape])

(defn y-or-n [prompt &optional [require-uppercase False]] (block
  (msgn "{} {}" prompt
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
  "KEY_DOWN" Pos.SOUTH
  "2" Pos.SOUTH
  "KEY_LEFT" Pos.WEST
  "4" Pos.WEST
  "KEY_RIGHT" Pos.EAST
  "6" Pos.EAST

  "KEY_HOME" Pos.NW
  "7" Pos.NW
  "KEY_PPAGE" Pos.NE
  "9" Pos.NE
  "KEY_END" Pos.SW
  "1" Pos.SW
  "KEY_NPAGE" Pos.SE
  "3" Pos.SE})

(defn input-direction [] (block
  (msgn "In what direction?")
  (rtv display.full-redraw)
  (setv G.last-new-message-number (dec (len G.message-log)))
  (while True
    (setv key (G.T.getkey))
    (when (in key direction-keys)
      (ret (get direction-keys key)))
    (when (in key cancel-keys)
      (ret None)))))

(defn get-normal-command [] (block
  (while True
    (setv key (G.T.getkey))
    (setv inp (cond

      [(= key G.key-escape)
        :quit-game]

      [(in key direction-keys)
        [:move (get direction-keys key)]]

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

      [(= key "R")
        :reset-level]))

    (when inp
      (ret inp)))))

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
            (msgn "You don't have such an item.")
            :quit))]

      [(in key cancel-keys)
        :quit]))

    (when (not (none? inp))
      (break)))

  (when (and (numeric? inp) (< inp (len G.inventory)))
    inp))
