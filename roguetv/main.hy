(require kodhy.macros)

(import
  sys textwrap
  curses
  [libtcodpy :as tcod]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat]])

;; * Parameters

(def MAP-WIDTH 80)
(def MAP-HEIGHT 40)

(def INVENTORY-LIMIT 10)

(def MESSAGE-LINES 3)

(def FG-COLOR :black)
(def BG-COLOR :white)
(def UNSEEN-COLOR :dark-gray)

(def NEW-MSG-HIGHLIGHT curses.A-BOLD)

(def KEY-ESCAPE "\x1b")

(def color-numbers {
  :black 16
  :white 15
  :light-gray 7
  :dark-gray 8
  :green 2
  :yellow 3})

;; * Declarations

(defclass X [object] [])
(def G (X))
  ; This object will store various global variables as attributes
  ; so we can elide 'global' declarations.

(def BOTTOM-BORDER (+ MESSAGE-LINES 1))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def SCREEN-WIDTH None)
(def SCREEN-HEIGHT None)
(def color-pairs {})
(def G.last-new-message-number -1)

; Times are in simulated sections.
(def G.current-time 0)
(def G.time-limit None)
(def G.last-action-duration 0)

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

(def inventory [])

;; * Creature

(defclass Creature [Drawable] [
  [extant []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (.__init__ (super Creature self) char color-fg color-bg)
    (set-self pos)
    (.append Creature.extant self)
    None)]])

(def player (kwc Creature
  :char "@" :color-bg :yellow
  :pos (Pos (/ MAP-WIDTH 2) (/ MAP-HEIGHT 2))))

;; * Input

(defn players-turn []

  (while True
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

      [(= key ":")
        [:examine-ground]]

      [(= key ",")
        [:pick-up]]

      [True
        [:retry-input]]))

    (unless (= (first inp) :retry-input)
      (break)))

  (setv G.last-new-message-number (dec (len message-log)))

  (setv [cmd args] [(first inp) (slice inp 1)])
  (cond

    [(= cmd :quit-game)
      :quit-game]

    [(= cmd :move)
      (let [[p-from player.pos] [p-to (+ p-from (first args))]]
        (if (and (on-map p-to) (not (. (mget p-to) blocks-movement)))
          (do
            (setv player.pos p-to)
            (recompute-fov)
            (describe-tile player.pos)
            (len-taxicab (first args)))
          0))]

    [(= cmd :examine-ground) (do
      (kwc describe-tile player.pos :+verbose)
      0)]

    [(= cmd :pick-up) (do
      (setv item (afind-or (= it.pos player.pos) Item.extant))
      (cond
        [(nil? item) (do
          (msg "There's nothing here to pick up.")
          0)]
        [(= (len inventory) INVENTORY-LIMIT) (do
          (msg "Your inventory is full.")
          0)]
        [True (do
          (msg (.format "You pick up {}." item.itype.name))
          (setv item.pos None)
          (.append inventory item)
          1)]))]

    [True
      0]))

;; * Messages

(def message-log [])
(defn msg [text]
  (.append message-log (, (len message-log) text)))

(defn describe-tile [pos &optional verbose]
  (setv item (afind-or (= it.pos pos) Item.extant))
  (cond
    [item
      (msg (.format "You see here {}." item.itype.name))]
    [verbose
      (msg "The floor is unremarkable.")]))

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
          (whenn (afind-or (= it.pos p) Item.extant)
            it.itype)
          (mget p))))
        (echo " " FG-COLOR UNSEEN-COLOR)))))

(defn draw-status-line []
  (T.addstr (- SCREEN-HEIGHT 1 MESSAGE-LINES) 0
    (.rjust (minsec (max 0 (- (or G.time-limit 0) G.current-time)))
      (len "10:00")))
  (when G.last-action-duration
    (T.addstr (.format " ({})" G.last-action-duration))))

(defn draw-bottom-message-log []
  (setv lines (concat
    (lc [[n text] (slice message-log (- MESSAGE-LINES))]
      (amap (, n it) (textwrap.wrap text SCREEN-WIDTH)))))
  (setv lines (slice lines (- MESSAGE-LINES)))
  (for [i (range MESSAGE-LINES)]
    (T.insstr (+ i (- SCREEN-HEIGHT MESSAGE-LINES)) 0
      (if (< i (len lines)) (get lines i 1) "")
      (and
        (< i (len lines))
        (> (get lines i 0) G.last-new-message-number)
        NEW-MSG-HIGHLIGHT))))

(defn full-redraw []
  (T.erase)
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

(setv toasters 15)
(for [x (range -2 3)]
  (for [y (range -2 3)]
    (setv p (+ player.pos (Pos x y)))
    (when (instance? Floor (mget p))
      (kwc Item :itype toaster :pos p)
      (-= toasters 1)
      (when (zero? toasters)
        (break)))))

(setv G.time-limit (* 2 60))

(recompute-fov)

(curses.wrapper (fn [scr]
  (global T) (global SCREEN-WIDTH) (global SCREEN-HEIGHT)

  (setv T scr)
  (setv [SCREEN-HEIGHT SCREEN-WIDTH] (T.getmaxyx))
  (curses.curs-set 0) ; Make the cursor invisible.
  (T.bkgd (ord " ") (default-color)) ; Set the background color.

  (while True
    (full-redraw)
    (setv result (players-turn))
    (cond

      [(= result :quit-game)
        (break)]

      [(numeric? result) (do
        (setv G.last-action-duration result)
        (when G.last-action-duration
          (+= G.current-time G.last-action-duration)
          (when (and G.time-limit (>= G.current-time G.time-limit))
            (msg "Time's up!")
            (setv G.time-limit None))))]

      [True
        (raise (ValueError (.format "Illegal players-turn result: {}" result)))]))))
