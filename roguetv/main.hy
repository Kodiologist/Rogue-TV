(require kodhy.macros)

(import
  sys os random string textwrap
  curses
  [libtcodpy :as tcod]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat ret]])

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

(defclass G [object] [])
(def G (G))
  ; This object will store various global variables as attributes
  ; so we can elide 'global' declarations.

(def BOTTOM-BORDER (+ MESSAGE-LINES 1))
  ; The extra 1 is for the status line.

(def T None) ; This will be set to a curses screen.
(def SCREEN-WIDTH None)
(def SCREEN-HEIGHT None)
(def color-pairs {})
(def G.last-new-message-number -1)

; Times are in simulated seconds.
(def G.current-time 0)
(def G.time-limit None)
(def G.last-action-duration 0)

;; * Utility

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

(defn shuffle [l]
  (setv l (list l))
  (random.shuffle l)
  l)

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

(defn room-for-creature? [pos]
  (and
    (on-map pos)
    (not (. (mget pos) blocks-movement))
    (not (Creature.at pos))))

(defn room-for-item? [pos]
  (and
    (on-map pos)
    (not (. (mget pos) blocks-movement))
    (not (Item.at pos))))

(defn recompute-fov []
  (kwc tcod.map-compute-fov fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range MAP-WIDTH)]
    (for [y (range MAP-HEIGHT)]
      (when (tcod.map-is-in-fov fov-map x y)
        (setv (get seen-map x y) True)))))

(defclass MapObject [object] [

  [init-omap (classmethod (fn [self width height]
    (setv self.omap (amap (* [None] height) (range width)))))]

  [__init__ (fn [self &optional pos]
    ; 'pos' may be None whenever the object isn't currently
    ; on the map.
    (setv self.pos None)
    (.move self pos)
    None)]

  [move (fn [self p-to]
    ; Set p-to to None to remove the object from the map.
    (when self.pos
      (setv (get self.omap self.pos.x self.pos.y) None))
    (when p-to
      (setv (get self.omap p-to.x p-to.y) self))
    (setv self.pos p-to))]

  [at (classmethod (fn [self pos]
    (get self.omap pos.x pos.y)))]])

;; * Item

(defclass ItemType [Drawable] [
  [defined []]

  [__init__ (fn [self tid name char &optional color-fg color-bg]
    (.__init__ (super ItemType self) char color-fg color-bg)
    (set-self tid name)
    (.append ItemType.defined self)
    None)]])

(defclass Item [MapObject] [

  [__init__ (fn [self itype &optional pos]
    (.__init__ (super Item self) pos)
    (set-self itype)
    None)]])

(def toaster (kwc ItemType
  :tid "toaster" :name "a toaster"
  :char "%" :color-fg :green))

(def inventory [])

;; * Creature

(defclass Creature [Drawable MapObject] [
  [extant []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (Drawable.__init__ self char color-fg color-bg)
    (MapObject.__init__ self pos)
    (.append Creature.extant self)
    None)]])

(def player (kwc Creature
  :char "@" :color-bg :yellow))

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

      [(= key "i")
        [:inventory]]
      [(= key ",")
        [:pick-up]]
      [(= key "d")
        [:drop]]))

    (when inp
      (break)))

  (setv G.last-new-message-number (dec (len message-log)))

  (setv [cmd args] [(first inp) (slice inp 1)])
  (block (cond

    [(= cmd :quit-game)
      :quit-game]

    [(= cmd :move)
      (let [[p-from player.pos] [p-to (+ p-from (first args))]]
        (unless (room-for-creature? p-to)
          (ret 0))
        (.move player p-to)
        (recompute-fov)
        (describe-tile player.pos)
        (len-taxicab (first args)))]

    [(= cmd :examine-ground) (do
      (kwc describe-tile player.pos :+verbose)
      0)]

    [(= cmd :inventory) (do
      (if inventory
        (kwc inventory-loop :!select "You are carrying:")
        (msg "Your inventory is empty."))
      0)]

    [(= cmd :pick-up) (do
      (setv item (Item.at player.pos))
      (when (nil? item)
        (msg "There's nothing here to pick up.")
        (ret 0))
      (when (= (len inventory) INVENTORY-LIMIT)
        (msg "Your inventory is full.")
        (msg (.format "(You can carry up to {} items.)" INVENTORY-LIMIT))
        (ret 0))
      (msg (.format "You pick up {}." item.itype.name))
      (.move item None)
      (.append inventory item)
      1)]

    [(= cmd :drop) (do
      (unless inventory
        (msg "You don't have anything to drop.")
        (ret 0))
      (setv i (inventory-loop "What do you want to drop?"))
      (when (none? i)
        ; Action canceled.
        (ret 0))
      (setv clear-spot (afind-or (room-for-item? it) (+
        ; Try to drop at the player's feet…
        [player.pos]
        ; …or at a random orthogonal neigbor…
        (shuffle (amap (+ player.pos it) Pos.ORTHS))
        ; …or at a random diagonal neighbor.
        (shuffle (amap (+ player.pos it) Pos.DIAGS)))))
      (unless clear-spot
        (msg "There's no room to drop anything here.")
        (ret 0))
      (setv item (.pop inventory i))
      (.move item clear-spot)
      (msg (.format "You drop {}." item.itype.name))
      1)]

    [True
      0])))

(defn inventory-loop [prompt &optional [select True]]

  (draw-inventory prompt)
  (T.refresh)

  (while True
    (setv key (T.getkey))
    (setv inp (cond

      [(and select (in key string.lowercase))
        (.index string.lowercase key)]

      [(in key [" " "\n" KEY-ESCAPE])
        :quit]))

    (unless (none? inp)
      (break)))

  (when (and (numeric? inp) (< inp (len inventory)))
    inp))

;; * Messages

(def message-log [])
(defn msg [text]
  (.append message-log (, (len message-log) text)))

(defn describe-tile [pos &optional verbose]
  (cond
    [(Item.at pos)
      (msg (.format "You see here {}." (. (Item.at pos) itype name)))]
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
          (Creature.at p)
          (whenn (Item.at p) it.itype)
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

(defn draw-inventory [prompt]
  (setv lines (+
    [[None prompt]]
    (amap
      [it (.format "  {} � {}" "x" it.itype.name)]
        ; The character � will be replaced with the item's symbol.
      inventory)
    (* [[None "      ---"]] (- INVENTORY-LIMIT (len inventory)))))
  (setv width (min SCREEN-WIDTH (inc (max (amap (len (second it)) lines)))))
  (for [[n [item text]] (enumerate lines)]
    (T.move n 0)
    (setv text (slice (.ljust text width) 0 width))
    (setv parts (.split text "�" 1))
    (T.addstr (first parts))
    (when (> (len parts) 1)
      (echo-drawable item.itype)
      (T.addstr (second parts)))))

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
(.init-omap Item MAP-WIDTH MAP-HEIGHT)
(.init-omap Creature MAP-WIDTH MAP-HEIGHT)
(.move player (Pos (/ MAP-WIDTH 2) (/ MAP-HEIGHT 2)))

(setv toasters 15)
(for [x (range -2 3)]
  (for [y (range -2 3)]
    (setv p (+ player.pos (Pos x y)))
    (when (room-for-item? p)
      (kwc Item :itype toaster :pos p)
      (-= toasters 1)
      (when (zero? toasters)
        (break)))))

(setv G.time-limit (* 2 60))

(recompute-fov)

(unless (in "ESCDELAY" os.environ)
  (setv (get os.environ "ESCDELAY") "10"))
    ; This ensures curses will respond to the escape key quickly
    ; in keypad mode (which is enabled by curses.wrapper).
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
