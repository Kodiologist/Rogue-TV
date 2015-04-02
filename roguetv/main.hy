(require kodhy.macros)

(import
  sys
  [libtcodpy :as tcod]
  curses
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat]])

;; * Parameters

(def MAP-WIDTH 80)
(def MAP-HEIGHT 40)

(def MESSAGE-LINES 3)

(setv UNSEEN-COLOR :dark-gray)

(def KEY-ESCAPE "\x1b")

;; * Declarations

(def BOTTOM-BORDER (+ MESSAGE-LINES 1))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def SCREEN-WIDTH None)
(def SCREEN-HEIGHT None)
(def color-pairs {})

(def time-left 0)  ; In simulated seconds.

;; * Utility

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

(defn minsec [s]
  (.format "{}:{:02}" (// s 60) (% s 60)))

(defn len-taxicab [p]
  (+ (abs p.x) (abs p.y)))

;; * Drawable

(defclass Drawable [object] [
  [__init__ (fn [self char &optional color-fg color-bg]
    (set-self char color-fg color-bg)
    None)]])

;; * Map

(defclass Tile [Drawable] [
  [__init__ (fn [self char &optional color-fg color-bg blocks-movement]
    (.__init__ (super Tile self) char color-fg color-bg)
    (set-self blocks-movement)
    None)]])

(defclass Floor [Tile] [
  [__init__ (fn [self]
    (kwc .__init__ (super Floor self)
      :char "."
      :!blocks-movement)
    None)]])

(defclass Wall [Tile] [
  [__init__ (fn [self]
    (kwc .__init__ (super Wall self)
      :char "#"
      :color-bg :black
      :+blocks-movement)
    None)]])

(def gmap
  (amap (amap None (range MAP-HEIGHT)) (range MAP-WIDTH)))

(defn mget [pos]
  (get gmap pos.x pos.y))

(defn mset [pos v]
  (setv (get gmap pos.x pos.y) v))

(defn on-map [pos]
  (and (<= 0 pos.x (dec MAP-WIDTH)) (<= 0 pos.y (dec MAP-HEIGHT))))

;; * Item

(defclass ItemType [Drawable] [
  [defined []]

  [__init__ (fn [self tid name char &optional color-fg color-bg]
    (.__init__ (super ItemType self) char color-fg color-bg)
    (set-self tid name)
    (.append ItemType.defined self)
    None)]])

(defclass Item [object] [
  [extant []]

  [__init__ (fn [self itype &optional pos]
    (set-self itype pos)
    (.append Item.extant self)
    None)]])

(def toaster (kwc ItemType
  :tid "toaster" :name "a toaster"
  :char "%" :color-fg :green))

;; * Creature

(defclass Creature [Drawable] [
  [extant []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (.__init__ (super Creature self) char color-fg color-bg)
    (set-self pos)
    (.append Creature.extant self)
    None)]])

(setv player (kwc Creature
  :char "@" :color-bg :yellow
  :pos (Pos (/ MAP-WIDTH 2) (/ MAP-HEIGHT 2))))

;; * Input

(defn players-turn []
  (setv key (T.getkey))

  (setv inp (cond

    [(= key KEY-ESCAPE)
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

    [True
      [:nop]]))

  (setv [cmd args] [(first inp) (slice inp 1)])
  (cond
    [(= cmd :move)
      (let [[p-from player.pos] [p-to (+ p-from (first args))]]
        (if (and (on-map p-to) (not (. (mget p-to) blocks-movement))) (do
          (setv player.pos p-to)
          [:moved args])
        (do ; else
          [:nop []])))]
     [True
       [cmd args]]))

;; * Display

(defn echo [str &optional [color-fg :black] [color-bg :white]]
  (T.addstr str (curses.color-pair (get color-pairs (, color-fg color-bg)))))

(defn ty->py [ty]
  (+ (- (// SCREEN-HEIGHT 2) ty) player.pos.y))
(defn tx->px [tx]
  (+ (- tx (// SCREEN-WIDTH 2)) player.pos.x))

(defn echo-drawable [d]
  (T.addstr d.char (curses.color-pair
    (get color-pairs (, (or d.color-fg :black) (or d.color-bg :white))))))

(defn draw-map []
  (T.move 0 0)
  (for [ty (range (- SCREEN-HEIGHT BOTTOM-BORDER))]
    (setv py (ty->py ty))
    (for [tx (range SCREEN-WIDTH)]
      (setv px (tx->px tx))
      (setv p (Pos px py))
      (if
        (and (<= 0 px (dec MAP-WIDTH)) (<= 0 py (dec MAP-HEIGHT))
          (get seen-map px py))
        (echo-drawable (or
          (afind-or (= it.pos p) Creature.extant)
          (let [[i (afind-or (= it.pos p) Item.extant)]]
            (and i i.itype))
          (mget p)))
        (kwc echo " " :color-bg UNSEEN-COLOR)))))

(defn draw-status-line []
  (setv text (if (<= time-left 0) "Game Over" (minsec time-left)))
  (T.insstr (- SCREEN-HEIGHT 1 MESSAGE-LINES) 0
    (+ text (* " " (- SCREEN-WIDTH (len text))))
    (curses.color-pair (get color-pairs (, :black :white)))))

(defn draw-bottom-message-log []
  (for [i (range MESSAGE-LINES)]
    (T.insstr (- SCREEN-HEIGHT (inc i)) 0
      (* " " (dec (* SCREEN-WIDTH MESSAGE-LINES)))
      (curses.color-pair (get color-pairs (, :black :white))))))

(defn recompute-fov []
  (kwc tcod.map-compute-fov fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range MAP-WIDTH)]
    (for [y (range MAP-HEIGHT)]
      (when (tcod.map-is-in-fov fov-map x y)
        (setv (get seen-map x y) True)))))

(defn full-redraw []
  (draw-status-line)
  (draw-bottom-message-log)
  (draw-map)
  (T.refresh))

(defn init-colors []
  (setv WHITE 15)
  (setv c {
    :black curses.COLOR-BLACK
    :white WHITE
    :light-gray 7
    :dark-gray 8
    :green curses.COLOR-GREEN
    :yellow 3})
  (setv l (sorted (frozenset (concat
    (lc [[k cn] (.items c)] [
      (, k      :white cn                 WHITE)
      (, :black k      curses.COLOR-BLACK cn)])))))
  (for [[i [fg-k bg-k fg-cn bg-cn]] (enumerate l)]
    (curses.init-pair (inc i) fg-cn bg-cn)
    (setv (get color-pairs (, fg-k bg-k)) (inc i))))

;; * Main loop

(def dugout (kwc heidegger.digger.generate-map
  :width MAP-WIDTH :height MAP-HEIGHT))
(def fov-map (tcod.map-new MAP-WIDTH MAP-HEIGHT))
(for [x (range MAP-WIDTH)]
  (for [y (range MAP-HEIGHT)]
    (mset (Pos x y) (if (get dugout "map" x y) (Wall) (Floor)))
    (tcod.map-set-properties fov-map x y
      (not (get dugout "map" x y))
      (not (get dugout "map" x y)))))
(def seen-map (amap (* [False] MAP-HEIGHT) (range MAP-WIDTH)))

(kwc Item :itype toaster :pos player.pos)

(setv time-left (* 2 60))

(recompute-fov)

(curses.wrapper (fn [scr]
  (global T) (global SCREEN-WIDTH) (global SCREEN-HEIGHT) (global time-left)
  (setv T scr)
  (setv [SCREEN-HEIGHT SCREEN-WIDTH] (T.getmaxyx))

  (curses.curs_set 0) ; Make the cursor invisible.

  (init-colors)

  (while True
    (full-redraw)
    (setv [result args] (players-turn))
    (when (= result :moved)
      (recompute-fov)
      (-= time-left (len-taxicab (first args))))
    (when (= result :quit-game)
      (break)))))
