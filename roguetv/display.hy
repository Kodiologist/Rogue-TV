(require kodhy.macros)

(import
  textwrap
  curses
  [libtcodpy :as tcod]
  [heidegger.pos [Pos]]
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [look-at-keys]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile Floor]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]])

(defn recompute-fov []
  (kwc tcod.map-compute-fov G.fov-map
    G.player.pos.x G.player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (when (tcod.map-is-in-fov G.fov-map x y)
        (setv (get G.seen-map x y) True)))))

(defn get-color [fg bg]
  (when (none? bg)
    (setv bg (G.pick-bg-color fg)))
  (curses.color-pair (try
    (get G.color-pairs (, fg bg))
    (catch [_ KeyError]
      ; This color pair hasn't been initialized yet. So do that.
      (setv i (+ 2 (len G.color-pairs)))
      (curses.init-pair i (get G.color-numbers fg) (get G.color-numbers bg))
      (setv (get G.color-pairs (, fg bg)) i)
      i))))

(defn default-color []
  (get-color G.fg-color G.bg-color))

(defn echo [str color-fg color-bg]
  (try
    (G.T.addstr str (get-color color-fg color-bg))
    (catch [_ curses.error] None)))
      ; http://bugs.python.org/issue8243

(defn echo-drawable [d]
  (echo d.char d.color-fg d.color-bg))

; The functions `tx->px` and `ty->py` map terminal coordinates to
; Pos (map coordinates). They center the view on the focus
; position (usually the player's position) except when the focus
; is near the edge of the map, in which case they only scroll
; `G.map-border-width` units off the map, so as not to waste
; screen real estate.
(defn tx->px [tx focus-px]
  (setv left (- focus-px (// G.screen-width 2)))
  (setv right (+ focus-px (// G.screen-width 2)))
  (cond
    [(>= G.screen-width (+ G.map-width (* 2 G.map-border-width)))
      (+ tx (- (// G.map-width 2) (// G.screen-width 2)))]
    [(< left (- G.map-border-width))
      (- tx G.map-border-width)]
    [(>= right (+ G.map-border-width G.map-width))
      (+ tx G.map-border-width G.map-width (- G.screen-width))]
    [True
      (+ (- tx (// G.screen-width 2)) focus-px)]))
(defn ty->py [ty focus-py]
  (setv bottom (- focus-py (// G.screen-height 2)))
  (setv top (+ focus-py (// G.screen-height 2)))
  (cond
    [(>= G.screen-height (+ G.map-height (* 2 G.map-border-width) G.bottom-border))
      (- (+ (// G.map-height 2) (// (- G.screen-height G.bottom-border) 2)) ty)]
    [(< bottom (- (+ G.map-border-width G.bottom-border)))
      (- G.screen-height 1 G.map-border-width G.bottom-border ty)]
    [(>= top (+ G.map-border-width G.map-height))
      (- (+ G.map-height G.map-border-width) 1 ty)]
    [True
      (+ (- (// G.screen-height 2) ty) focus-py)]))

(defn draw-map [focus ty-min ty-max]
  (when G.fov-dirty?
    (recompute-fov)
    (setv G.fov-dirty? False))
  (G.T.move 0 ty-min)
  (for [ty (seq ty-min ty-max)]
    (setv py (ty->py ty focus.y))
    (for [tx (range G.screen-width)]
      (setv px (tx->px tx focus.x))
      (when (= (Pos px py) focus)
        (setv focus-t-coords [ty tx]))
      (cond
        [(not (and (<= 0 px (dec G.map-width)) (<= 0 py (dec G.map-height))))
          ; Off the map.
          (echo " " G.fg-color G.off-map-color)]
        [(get G.seen-map px py)
          ; Seen by the player.
          (echo-drawable (let [[p (Pos px py)]] (or
            (Creature.at p)
            (Item.at p)
            (Tile.at p))))]
        [True
          ; Unseen.
          (echo " " G.fg-color G.unseen-color)])))
  focus-t-coords)

(defn draw-status-line []
  (G.T.addstr (- G.screen-height 1 G.message-lines) 0
    (.format "{} {}  DL:{: 2}  {:>6}{}"
      (.rjust (minsec (max 0 (- (or G.time-limit 0) G.current-time)))
        (len "10:00"))
      (.ljust
        (if G.last-action-duration
         (.format "({})" (show-round G.last-action-duration 2))
          "")
        (len "(1.15)"))
      G.dungeon-level
      (+ "$" (string (sum
        (fmap (.apparent-price it) (.identified? it) G.inventory))))
      (if (afind-or (not (.identified? it)) G.inventory)
        " + ?"
        "    "))))

(defn draw-bottom-message-log []
  (setv lines (concat
    (lc [[mn mtype text] (slice G.message-log (- G.message-lines))]
      (amap (, mn mtype it) (kwc textwrap.wrap :width G.screen-width (cond
        [(none? mtype)   text]
        [(= mtype :tara) (+ "Tara: " text)]
        [(= mtype :bob)  (+ "Bob: " text)]))))))
  (setv lines (slice lines (- G.message-lines)))
  (for [[i [mn mtype text]] (enumerate lines)]
    (G.T.move (+ i (- G.screen-height G.message-lines)) 0)
    (setv attr (and (> mn G.last-new-message-number)
      G.new-msg-highlight))
    (for [[kw t] [[:tara "Tara: "] [:bob "Bob: "]]]
      (when (and (= mtype kw) (.startswith text t))
        (G.T.addstr t (| attr (get-color (get G.announcer-colors kw) G.bg-color)))
        (setv text (slice text (len t)))))
    (G.T.insstr text attr)))

(defn full-redraw [&optional focus]
  (G.T.erase)
  (setv focus-t-coords (kwc draw-map
    :focus (or focus G.player.pos)
    :ty-min 0
    :ty-max (if (= G.screen-mode :normal)
      (dec (- G.screen-height G.bottom-border))
      (dec G.screen-height))))
  (when (= G.screen-mode :normal)
    (draw-status-line)
    (draw-bottom-message-log)
    (curses.curs-set 0))
  (when (= G.screen-mode :look)
    (draw-look-legend focus)
    (curses.curs-set 1)
    (apply G.T.move focus-t-coords))
  (G.T.refresh))

(defn draw-text-screen [text]
  (curses.curs-set 0)
  (G.T.erase)
  (setv w (- G.screen-width G.text-screen-left-margin))
  (for [[i line] (enumerate (slice
      (concat (amap
        (if it (kwc textwrap.wrap it :width w) [""])
        (.split text "\n")))
      0 (dec G.screen-height)))]
    (G.T.move i G.text-screen-left-margin)
    (G.T.insstr line)))

(defn draw-inventory [prompt]
  (setv names (amap (.format "{:a:most}" it) G.inventory))
  (setv prices (amap (.apparent-price it) G.inventory))
  (setv lines (+
    [[None prompt]]
    (amap
      [(get G.inventory it) (.format "  {} � {:{}}  {}{:>{}}"
        ; The character � will be replaced with the item's symbol.
        (. (get G.inventory it) invlet)
        (get names it)
        (max (map len names))
        (if (zero? it) "$" " ")
        (get prices it)
        (max (amap (len (string it)) prices)))]
      (range (len G.inventory)))
    (* [[None "      ---"]] (- G.inventory-limit (len G.inventory)))))
  (setv width (min G.screen-width (inc (max (amap (len (second it)) lines)))))
  (for [[n [item text]] (enumerate lines)]
    (G.T.move n 0)
    (setv text (slice (.ljust text width) 0 width))
    (setv parts (.split text "�" 1))
    (G.T.addstr (first parts))
    (when (> (len parts) 1)
      (echo-drawable item)
      (G.T.addstr (second parts)))))

(defn draw-look-legend [p]
  ; In look mode, show a legend describing the creature, item,
  ; and tile under the cursor.
  (setv dunno (unless (get G.seen-map p.x p.y) (, None "    ? unseen")))
  (setv lines [
    [None "At cursor: (press a key to examine)"]
    ; � characters will be replaced with map symbols.
    (or dunno (whenn (Creature.at p)
      (, it (.format "  {} � {:a}" (get look-at-keys :creature) it))))
    (or dunno (whenn (Item.at p)
      (, it (.format "  {} � {:a:full}" (get look-at-keys :item) it))))
    (or dunno
      (, (Tile.at p) (.format "  {} � {:a}" (get look-at-keys :tile) (Tile.at p))))])
  (setv lines (amap (or it (, None "      ---")) lines))
  (assert (= (len lines) G.look-mode-legend-height))
  (setv width (min G.screen-width (inc (max (amap (len (second it)) lines)))))
  (for [[n [drawable text]] (enumerate lines)]
    (G.T.move (- G.screen-height (- (len lines) n)) 0)
    (setv text (slice (.ljust text width) 0 width))
    (setv parts (.split text "�" 1))
    (G.T.addstr (first parts))
    (when (> (len parts) 1)
      (echo-drawable drawable)
      (G.T.addstr (second parts)))))

(defn describe-tile [pos &optional verbose]
  (setv tile (Tile.at pos))
  (cond
    [(Item.at pos) (do
      (msg "You see here {:a:full}." (Item.at pos))
      (unless (instance? Floor tile)
        ; This triggers even when 'verbose' is false because
        ; there's an item covering this tile, so the tile type
        ; may not be obvious.
        (msg "There is also {:a} here." tile)))]
    [verbose
      (if (instance? Floor tile)
        (msg :bob "Now the beetle-headed {} is snilching the floor. Wonder what {p:he's} looking for."
          (if (G.player.female) "dowdy" "cull"))
        (msg "There is {:a} here." tile))]))
