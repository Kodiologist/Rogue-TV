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

(def invlets (list "abcdefghijklmnopqrstuvwxyz"))

(def color-numbers {
  :black 16
  :white 15
  :light-gray 7
  :dark-gray 8
  :green 2
  :blue 12
  :red 9
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

(def G.endgame False)
(def G.dungeon-level None)

; Times are in simulated seconds.
(def G.current-time 0)
(def G.time-limit None)
(def G.last-action-duration 0)

(def G.fov-map (tcod.map-new MAP-WIDTH MAP-HEIGHT))
(def seen-map [])

;; * Utility

(defmacro set-self [&rest props]
  `(do ~@(amap `(setv (. self ~it) ~it)  props)))

(defmacro set-self-nn [&rest props]
  `(do ~@(amap `(unless (none? ~it) (setv (. self ~it) ~it))  props)))

(defn shuffle [l]
  (setv l (list l))
  (random.shuffle l)
  l)

(defn pick [l]
  (first (random.sample l 1)))

(defn randpop [l]
  (l.pop (random.randrange (len l))))

(defn minsec [s]
  (.format "{}:{:02}" (// s 60) (% s 60)))

(defn len-taxicab [p]
  (+ (abs p.x) (abs p.y)))

;; * Drawable

(defclass Drawable [object] [
  [char None]
  [color-fg FG-COLOR]
  [color-bg BG-COLOR]])

;; * Map

(defclass Tile [Drawable] [
  [description None]
  [blocks-movement False]

  [use-tile (fn [self]
    ; The player has used the command :use-tile on top of this
    ; tile. Do whatever is necessary and return the time
    ; taken.
    ;
    ; The default implementaton does nothing.
    (msg "There's nothing special you can do at this tile.")
    0)]])

(defclass Floor [Tile] [
  [char "."]])

(defclass Wall [Tile] [
  [description "a wall"]
  [char "#"]
  [color-bg FG-COLOR]
  [blocks-movement True]])

(defclass Elevator [Tile] [])

(defclass UpElevator [Elevator] [
  [description "an elevator going up"]
  [char "<"]

  [use-tile (fn [self]
    (msg "Taking the elevator up will end the game, but you get to keep what you have.")
    (when (y-or-n "Take the elevator up?" :+require-uppercase)
      (setv G.endgame :used-up-elevator))
    0)]])

(defclass DownElevator [Elevator] [
  [description "an elevator going down"]
  [char ">"]

  [use-tile (fn [self]
    (+= G.dungeon-level 1)
    (reset-level)
    (recompute-fov)
    0)]])

(def gmap
  (amap (amap None (range MAP-HEIGHT)) (range MAP-WIDTH)))

(defn mget [pos]
  (get gmap pos.x pos.y))

(defn mset [pos v]
  (setv (get gmap pos.x pos.y) v))

(defn on-map [pos]
  (and (<= 0 pos.x (dec MAP-WIDTH)) (<= 0 pos.y (dec MAP-HEIGHT))))

(defn recompute-fov []
  (kwc tcod.map-compute-fov G.fov-map
    player.pos.x player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range MAP-WIDTH)]
    (for [y (range MAP-HEIGHT)]
      (when (tcod.map-is-in-fov G.fov-map x y)
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

(defn room-for? [mo-class pos]
  (and
    (on-map pos)
    (not (. (mget pos) blocks-movement))
    (not (.at mo-class pos))))

(defn reset-level []
  (setv dugout (kwc heidegger.digger.generate-map
    :width MAP-WIDTH :height MAP-HEIGHT))

  (setv free-floors [])
  (for [x (range MAP-WIDTH)]
    (for [y (range MAP-HEIGHT)]
      (setv p (Pos x y))
      (setv floor? (not (get dugout "map" x y)))
      (when floor?
        (.append free-floors p))
      (mset p (if floor? (Floor) (Wall)))
      (tcod.map-set-properties G.fov-map x y floor? floor?)))

  (setv upelv-pos (Pos (/ MAP-WIDTH 2) (/ MAP-HEIGHT 2)))
  (mset upelv-pos (UpElevator))
  (.remove free-floors upelv-pos)
  (mset (randpop free-floors) (DownElevator))

  (setv (slice seen-map) (amap (* [False] MAP-HEIGHT) (range MAP-WIDTH)))
  (.init-omap Item MAP-WIDTH MAP-HEIGHT)
  (.init-omap Creature MAP-WIDTH MAP-HEIGHT)
  ; Now that we're on a new level, the positions of old
  ; MapObjects are invalid. But that's okay because there's no
  ; way to refer to old MapObjects anymore, either (except for
  ; ones with pos None, like the items in the player's
  ; inventory).

  (setv G.time-limit (+ G.current-time (* 5 60)))

  (.move player upelv-pos))

;; * Item

(def itypes {})

(defclass ItemType [Drawable] [

  [__init__ (fn [self tid name char &optional color-fg color-bg]
    (set-self tid name char)
    (set-self-nn color-fg color-bg)
    (setv (get itypes tid) self)
    None)]])

(defclass Item [MapObject] [

  [__init__ (fn [self itype &optional pos invlet]
    (.__init__ (super Item self) pos)
    (set-self itype invlet)
    None)]

  [invstr (fn [self]
    (.format "{} - {}"
      self.invlet
      self.itype.name))]])

(kwc ItemType
  :tid "toaster" :name "a toaster"
  :char "%" :color-fg :green)
(kwc ItemType
  :tid "galoshes" :name "a pair of galoshes"
  :char "[" :color-fg :yellow)
(kwc ItemType
  :tid "ottoman" :name "an ottoman"
  :char "_" :color-fg :red)
(kwc ItemType
  :tid "food-processor" :name "a food processor"
  :char "+" :color-fg :blue)

(def inventory [])

(defn add-to-inventory [item]
  (.move item None)
  (setv il-in-use (amap it.invlet inventory))
  (when (or (not item.invlet) (in item.invlet il-in-use))
    ; Assign the oldest invlet not used for an item already in
    ; the inventory.
    (setv item.invlet (afind-or (not-in it il-in-use) invlets))
    ; Move this invlet to the end of 'invlets' (since it's now
    ; the most recently used).
    (invlets.remove item.invlet)
    (invlets.append item.invlet))
  (.append inventory item))

;; * Creature

(defclass Creature [Drawable MapObject] [
  [extant []]

  [__init__ (fn [self &optional char color-fg color-bg pos]
    (MapObject.__init__ self pos)
    (set-self char)
    (set-self-nn color-fg color-bg)
    (.append Creature.extant self)
    None)]])

(def player (kwc Creature
  :char "@" :color-bg :yellow))

;; * Input

(defn y-or-n [prompt &optional [require-uppercase False]] (block
  (msg "{} {}" prompt
    (if require-uppercase "(Y/N; case-sensitive)" "(y/n)"))
  (full-redraw)
  (setv G.last-new-message-number (dec (len message-log)))
  (while True
    (setv key (T.getkey))
    (unless require-uppercase
      (setv key (.upper key)))
    (when (= key "Y")
      (ret True))
    (when (= key "N")
      (ret False)))))

(defn hit-key-to-continue [keys]
  (while True
    (when (in (T.getkey) keys)
      (break))))

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
      [(= key "t")
        [:use-tile]]

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
        (unless (room-for? Creature p-to)
          (ret 0))
        (.move player p-to)
        (recompute-fov)
        (describe-tile player.pos)
        (len-taxicab (first args)))]

    [(= cmd :examine-ground) (do
      (kwc describe-tile player.pos :+verbose)
      0)]

    [(= cmd :use-tile)
      (.use-tile (mget player.pos))]

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
        (msg "Your inventory is full. (You can carry up to {} items.)"
          INVENTORY-LIMIT)
        (ret 0))
      (add-to-inventory item)
      (msg "Taken:  {}" (item.invstr))
      1)]

    [(= cmd :drop) (do
      (unless inventory
        (msg "You don't have anything to drop.")
        (ret 0))
      (setv i (inventory-loop "What do you want to drop?"))
      (when (none? i)
        ; Action canceled.
        (ret 0))
      (setv clear-spot (afind-or (room-for? Item it) (+
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
      (msg "Dropped:  {}" (item.invstr))
      1)]

    [True
      0])))

(defn inventory-loop [prompt &optional [select True]]

  (draw-inventory prompt)
  (T.refresh)

  (setv il (amap it.invlet inventory))

  (while True
    (setv key (T.getkey))
    (setv inp (cond

      [(and select (in key invlets))
        (if (in key il)
          (.index il key)
          (do
            (msg "You don't have such an item.")
            :quit))]

      [(in key [" " "\n" KEY-ESCAPE])
        :quit]))

    (unless (none? inp)
      (break)))

  (when (and (numeric? inp) (< inp (len inventory)))
    inp))

;; * Messages

(def message-log [])
(defn msg [&rest format-args]
  (.append message-log (,
    (len message-log)
    (apply .format format-args))))

(defn describe-tile [pos &optional verbose]
  (setv tile (mget pos))
  (cond
    [(Item.at pos) (do
      (msg "You see here {}." (. (Item.at pos) itype name))
      (unless (instance? Floor tile)
        ; This triggers even when 'verbose' is false because
        ; there's an item covering this tile, so the tile type
        ; may not be obvious.
        (msg "There is also {} here." tile.description)))]
    [verbose
      (if (instance? Floor tile)
        (msg "The floor is unremarkable.")
        (msg "There is {} here." tile.description))]))

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
  (echo d.char d.color-fg d.color-bg))

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
    (.format "{} {}  DL:{: 2}"
      (.rjust (minsec (max 0 (- (or G.time-limit 0) G.current-time)))
        (len "10:00"))
      (if G.last-action-duration
        (.format "({})" G.last-action-duration)
        "   ")
      G.dungeon-level)))

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
      [it (.format "  {} � {}" it.invlet it.itype.name)]
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

(setv G.dungeon-level 1)
(reset-level)

(setv starting-items 15)
(for [x (range -2 3)]
  (for [y (range -2 3)]
    (setv p (+ player.pos (Pos x y)))
    (when (room-for? Item p)
      (kwc Item :itype (pick (.values itypes)) :pos p)
      (-= starting-items 1)
      (when (zero? starting-items)
        (break)))))

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

  (describe-tile player.pos)

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
            (setv G.time-limit None)
            (setv G.endgame :out-of-time))))]

      [True
        (raise (ValueError (.format "Illegal players-turn result: {}" result)))])

    (when G.endgame
      (msg "Game over. Press Escape to quit.")
      (full-redraw)
      (hit-key-to-continue [KEY-ESCAPE])
      (break)))))
