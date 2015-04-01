(require kodhy.macros)

(import
  sys
  [libtcodpy :as tcod]
  blessed
  [heidegger.pos [Pos]]
  heidegger.digger)

;; * Parameters

(def MAP-WIDTH 80)
(def MAP-HEIGHT 40)

(def MESSAGE-LINES 3)

;; * Declarations

(def BOTTOM-BORDER (+ MESSAGE-LINES 1))
  ; The extra 1 is for the status line.

(def T (blessed.Terminal))

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

(setv UnseenSquare (kwc Drawable
  :char " "
  :color-bg T.on-bright-black))

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
      :color-bg T.on-black
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
  :char "%" :color-fg T.green))

;; * Creature

(defclass Creature [Drawable] [
  [extant []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (.__init__ (super Creature self) char color-fg color-bg)
    (set-self pos)
    (.append Creature.extant self)
    None)]])

(setv player (kwc Creature
  :char "@" :color-bg T.on-bright-yellow
  :pos (Pos (/ MAP-WIDTH 2) (/ MAP-HEIGHT 2))))

;; * Input

(defn players-turn []
  (setv key (T.inkey))

  (setv inp (cond

    [(= key.code T.KEY-ESCAPE)
      [:quit-game]]

    [(or (= key.code T.KEY-UP) (= key "8"))
      [:move Pos.NORTH]]
    [(or (= key.code T.KEY-DOWN) (= key "2"))
      [:move Pos.SOUTH]]
    [(or (= key.code T.KEY-LEFT) (= key "4"))
      [:move Pos.WEST]]
    [(or (= key.code T.KEY-RIGHT) (= key "6"))
      [:move Pos.EAST]]

    [(or (= key.code T.KEY-HOME) (= key "7"))
      [:move Pos.NW]]
    [(or (= key.code T.KEY-PGUP) (= key "9"))
      [:move Pos.NE]]
    [(or (= key.code T.KEY-END) (= key "1"))
      [:move Pos.SW]]
    [(or (= key.code T.KEY-PGDN) (= key "3"))
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

(defn echo [&rest args]
  (apply print args {"end" "" "sep" ""}))

(defn ty->py [ty]
  (+ (- (// T.height 2) ty) player.pos.y))
(defn tx->px [tx]
  (+ (- tx (// T.width 2)) player.pos.x))

(defn echo-drawable [d]
  (echo
    ((or d.color-fg identity)
      ((or d.color-bg identity)
        d.char))))

(defn draw-map []
  (echo (T.move 0 0))
  (for [ty (range (- T.height BOTTOM-BORDER))]
    (setv py (ty->py ty))
    (for [tx (range T.width)]
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
        (echo (UnseenSquare.color-bg " "))))))

(defn draw-status-line []
  (echo
    (T.move (- T.height 1 MESSAGE-LINES) 0)
    (if (<= time-left 0) "Game Over" (minsec time-left))))

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
  (draw-map)
  (.flush sys.stdout))

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

(with [[(T.hidden-cursor)] [(T.cbreak)] [(T.fullscreen)]]

  (while True
    (full-redraw)
    (setv [result args] (players-turn))
    (when (= result :moved)
      (recompute-fov)
      (-= time-left (len-taxicab (first args))))
    (when (= result :quit-game)
      (break))))
