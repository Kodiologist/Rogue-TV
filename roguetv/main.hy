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

(setv FG-COLOR :black)
(setv BG-COLOR :white)
(setv UNSEEN-COLOR :dark-gray)

(def KEY-ESCAPE "\x1b")

(setv color-numbers {
  :black 0
  :white 15
  :light-gray 7
  :dark-gray 8
  :green 2
  :yellow 3})

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
      :color-bg FG-COLOR
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

(defn recompute-fov []
  (kwc tcod.map-compute-fov fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range MAP-WIDTH)]
    (for [y (range MAP-HEIGHT)]
      (when (tcod.map-is-in-fov fov-map x y)
        (setv (get seen-map x y) True)))))

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

(def color-pairs {})
(defn get-color [fg bg]
  (curses.color-pair (try
    (get color-pairs (, fg bg))
    (catch [_ KeyError]
      ; This color pair hasn't been initialized yet. So do that.
      (setv i (+ 2 (len color-pairs)))
      (curses.init-pair i (get color-numbers fg) (get color-numbers bg))
      (setv (get color-pairs (, fg bg)) i)
      i))))

(defn default-color []
  (get-color FG-COLOR BG-COLOR))

(defn echo [str color-fg color-bg]
  (T.addstr str (get-color color-fg color-bg)))

(defn echo-drawable [d]
  (echo d.char (or d.color-fg FG-COLOR) (or d.color-bg BG-COLOR)))

(defn ty->py [ty]
  (+ (- (// SCREEN-HEIGHT 2) ty) player.pos.y))
(defn tx->px [tx]
  (+ (- tx (// SCREEN-WIDTH 2)) player.pos.x))

(defn draw-map []
  (T.move 0 0)
  (for [ty (range (- SCREEN-HEIGHT BOTTOM-BORDER))]
    (setv py (ty->py ty))
    (for [tx (range SCREEN-WIDTH)]
      (setv px (tx->px tx))
      (if
        (and (<= 0 px (dec MAP-WIDTH)) (<= 0 py (dec MAP-HEIGHT))
          (get seen-map px py))
        (echo-drawable (let [[p (Pos px py)]] (or
          (afind-or (= it.pos p) Creature.extant)
          (let [[i (afind-or (= it.pos p) Item.extant)]]
            (and i i.itype))
          (mget p))))
        (echo " " FG-COLOR UNSEEN-COLOR)))))

(defn draw-status-line []
  (setv text (if (<= time-left 0) "Game Over" (minsec time-left)))
  (T.insstr (- SCREEN-HEIGHT 1 MESSAGE-LINES) 0
    (+ text (* " " (- SCREEN-WIDTH (len text))))
    (default-color)))

(defn draw-bottom-message-log []
  (for [i (range MESSAGE-LINES)]
    (T.insstr (- SCREEN-HEIGHT (inc i)) 0
      (* " " (dec (* SCREEN-WIDTH MESSAGE-LINES)))
      (default-color))))

(defn full-redraw []
  (draw-status-line)
  (draw-bottom-message-log)
  (draw-map)
  (T.refresh))

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

  (while True
    (full-redraw)
    (setv [result args] (players-turn))
    (when (= result :moved)
      (recompute-fov)
      (-= time-left (len-taxicab (first args))))
    (when (= result :quit-game)
      (break)))))
