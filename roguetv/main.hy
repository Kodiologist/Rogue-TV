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

(defn term-coords [pos]
  (, (+ (- pos.x player.pos.x) (// T.width 2))
    (+ (- player.pos.y pos.y) (// T.height 2))))

(defn on-screen [tx ty]
  (and (<= 0 tx (- T.width 1))
    (<= 0 ty (- T.height 1 BOTTOM-BORDER))))

(defn echo-drawable [d pos]
  (setv [px py] [pos.x pos.y])
  (setv [tx ty] (term-coords pos))
  (when (tcod.map-is-in-fov fov-map px py)
    (setv (get seen-map px py) True))
  (when (on-screen tx ty)
    (unless (get seen-map px py)
      (setv d UnseenSquare))
    (def char (or d.char "?"))
    (def color-fg (or d.color-fg T.black))
    (def color-bg (or d.color-bg T.on-bright-white))
    (echo (T.move ty tx) (color-fg (color-bg char)))))

(defn draw-map []
  ; Draw all the map tiles first.
  (for [y (range MAP-HEIGHT)]
    (for [x (range MAP-WIDTH)]
      (echo-drawable (mget (Pos x y)) (Pos x y))))
  ; Now draw all the items on the map.
  (for [item Item.extant]
    (when item.pos
      (echo-drawable item.itype item.pos)))
  ; Now draw the creatures.
  (for [cr Creature.extant]
    (when cr.pos
      (echo-drawable cr cr.pos))))

(defn draw-status-line []
  (echo
    (T.move (- T.height 1 MESSAGE-LINES) 0)
    (if (<= time-left 0) "Game Over" (minsec time-left))))

(defn full-redraw []
  (echo (T.clear))
  (draw-status-line)
  (kwc tcod.map-compute-fov fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
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

(with [[(T.hidden-cursor)] [(T.cbreak)] [(T.fullscreen)]]

  (while True
    (full-redraw)
    (setv [result args] (players-turn))
    (when (= result :moved)
      (-= time-left (len-taxicab (first args))))
    (when (= result :quit-game)
      (break))))
