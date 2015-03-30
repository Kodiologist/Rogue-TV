(require kodhy.macros)

(import
  sys
  [libtcodpy :as tcod]
  blessed
  [heidegger.pos [Pos]]
  heidegger.digger)

;; * Parameters

(def MAP-WIDTH 60)
(def MAP-HEIGHT 17)

(def BORDER-Y 1)
(def BORDER-X 2)

;; * Declarations

(def T (blessed.Terminal))

;; * Utility

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

;; * Map

(defclass Tile [object] [
  [__init__ (fn [self &optional char color-fg color-bg blocks-movement]
    (set-self char color-fg color-bg blocks-movement)    
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
      :+blocks-movement
      :color-bg T.on-black)
    None)]])

(defclass UnseenSquare [object] [ ; Not a real tile.
  [char " "]
  [color-fg None]
  [color-bg T.on-bright-black]])

(def gmap
  (amap (amap None (range MAP-HEIGHT)) (range MAP-WIDTH)))

(defn mget [pos]
  (get gmap pos.x pos.y))

(defn mset [pos v]
  (setv (get gmap pos.x pos.y) v))

(defn on-map [pos]
  (and (<= 0 pos.x (dec MAP-WIDTH)) (<= 0 pos.y (dec MAP-HEIGHT))))

;; * Creature

(defclass Creature [object] [
  [alive []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (set-self char color-fg color-bg pos)
    (.append Creature.alive self)
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
    [(or (in key.code [T.KEY-UP T.KEY-KP-8]) (= key "8"))
      [:move Pos.NORTH]]
    [(or (in key.code [T.KEY-DOWN T.KEY-KP-2]) (= key "2"))
      [:move Pos.SOUTH]]
    [(or (in key.code [T.KEY-LEFT T.KEY-KP-4]) (= key "4"))
      [:move Pos.WEST]]
    [(or (in key.code [T.KEY-RIGHT T.KEY-KP-6]) (= key "6"))
      [:move Pos.EAST]]
    [True
      [:nop]]))

  (setv [cmd args] [(first inp) (slice inp 1)])
  (cond
    [(= cmd :move)
      (let [[p-from player.pos] [p-to (+ p-from (first args))]]
        (when (and (on-map p-to) (not (. (mget p-to) blocks-movement)))
          (setv player.pos p-to)))])

  cmd)

;; * Display

(defn echo [&rest args]
  (apply print args {"end" "" "sep" ""}))

(defn echo-thing [x y t]
  (when (tcod.map-is-in-fov fov-map x y)
    (setv (get seen-map x y) True))
  (unless (get seen-map x y)
    (setv t UnseenSquare))
  (def char (or t.char "?"))
  (def color-fg (or t.color-fg T.black))
  (def color-bg (or t.color-bg T.on-bright-white))
  (echo (color-fg (color-bg char))))

(defn cursor-to-pos [pos]
  (echo (T.move
    (+ BORDER-Y (- MAP-HEIGHT 1 pos.y))
    (+ BORDER-X pos.x))))

(defn redraw-map []
  ; Draw all the map tiles first.
  (for [y (range MAP-HEIGHT)]
    (cursor-to-pos (Pos 0 y))
    (for [x (range MAP-WIDTH)]
      (echo-thing x y (mget (Pos x y)))))
  ; Now draw all the creatures on the map.
  (for [cr Creature.alive]
    (cursor-to-pos cr.pos)
    (echo-thing cr.pos.x cr.pos.y cr)))

(defn full-redraw []
  (kwc tcod.map-compute-fov fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
  (redraw-map))

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

(with [[(T.hidden-cursor)] [(T.cbreak)] [(T.fullscreen)]]

  (while True
    (full-redraw)
    (.flush sys.stdout)
    (setv cmd (players-turn))
    (when (= cmd :quit-game)
      (break))))
