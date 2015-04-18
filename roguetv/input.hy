(require kodhy.macros roguetv.macros)

(import
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

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

(defn get-normal-command [] (block
  (while True
    (setv key (G.T.getkey))
    (setv inp (cond

      [(= key G.key-escape)
        [:quit-game]]

      [(in key ["KEY_UP" "8"])
        [:move Pos.NORTH]]
      [(in key ["KEY_DOWN" "2"])
        [:move Pos.SOUTH]]
      [(in key ["KEY_LEFT" "4"])
        [:move Pos.WEST]]
      [(in key ["KEY_RIGHT" "6"])
        [:move Pos.EAST]]

      [(in key ["KEY_HOME" "7"])
        [:move Pos.NW]]
      [(in key ["KEY_PPAGE" "9"])
        [:move Pos.NE]]
      [(in key ["KEY_END" "1"])
        [:move Pos.SW]]
      [(in key ["KEY_NPAGE" "3"])
        [:move Pos.SE]]

      [(= key ":")
        [:examine-ground]]
      [(= key "t")
        [:use-tile]]

      [(= key "i")
        [:inventory]]
      [(= key ",")
        [:pick-up]]
      [(= key "d")
        [:drop]]

      [(= key "R")
        [:reset-level]]))

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

      [(in key [" " "\n" G.key-escape])
        :quit]))

    (when (not (none? inp))
      (break)))

  (when (and (numeric? inp) (< inp (len G.inventory)))
    inp))
