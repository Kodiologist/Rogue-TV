(require kodhy.macros roguetv.macros)

(import
  [math [floor ceil]]
  textwrap
  curses
  [heidegger.pos [Pos]]
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [look-at-keys]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile Floor]]
  [roguetv.fov [recompute-fov]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]]
  [roguetv.attrstr [AttrStr get-color curses-encode]])

(defn addstr [a1 &optional a2 a3 a4]
  (try
    (cond
      [(not (none? a4))
        (G.T.addstr a1 a2 (curses-encode a3) a4)]
      [(not (none? a3))
        (G.T.addstr a1 a2 (curses-encode a3))]
      [(not (none? a2))
        (G.T.addstr (curses-encode a1) a2)]
      [True
        (G.T.addstr (curses-encode a1))])
    (catch [_ curses.error] None)))
      ; http://bugs.python.org/issue8243

(defn echo [str color-fg color-bg]
  (addstr str (get-color color-fg color-bg)))

(defn echo-drawable [d]
  (echo (.get-char d) (.get-color-fg d) (.get-color-bg d)))

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
  (-= focus-py (int (ceil (/ G.bottom-border 2))))
    ; Adjust the focus so the player usually appears centered.
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
            (.visible-item-at G.player p)
            (Tile.at p))))]
        [True
          ; Unseen.
          (echo " " G.fg-color G.unseen-color)])))
  focus-t-coords)

(defn draw-status-line []
  (setv time-left (max 0 (- (or G.time-limit 0) G.current-time)))
  (setv time-left-frac (/ time-left (dl-time-limit G.dungeon-level)))
  (setv s (AttrStr.from-xml
    (.format "{} {} {}  DL:{: 2}  {:>6}{}  {}"
      (color-xml
        (let [[w (* G.time-bar-width time-left-frac)]]
          (cat
            (* (get G.time-bar-chunk-chars -1) (int (floor w)))
            (when (% w 1) (get G.time-bar-chunk-chars (int (round
              (* (% w 1) (dec (len G.time-bar-chunk-chars)))))))
            (* (get G.time-bar-chunk-chars 0) (- G.time-bar-width (int (ceil w))))))
        None
        (whenn (afind-or (<= time-left-frac (first it)) G.time-warnings)
          (second it)))
      (.rjust (minsec time-left) (len "10:00"))
      (.ljust
        (if G.last-action-duration
         (.format "({})" (show-round
           (/ G.last-action-duration G.clock-factor)
           3))
          "")
        (len "(1.151)"))
      (inc G.dungeon-level)
      (+ "$" (string (sum
        (fmap (.apparent-price it) (.identified? it) G.inventory))))
      (if (afind-or (not (.identified? it)) G.inventory)
        " + ?"
        "    ")
      (.join " " (amap it.status G.player.effects)))))
  (G.T.move (- G.screen-height 1 G.message-lines) 0)
  (.draw (.trunc s G.screen-width)))

(defn draw-message-log [&optional [fullscreen False]]
  (when fullscreen
    (G.T.erase))
  (setv height (if fullscreen G.screen-height G.message-lines))
  (setv lines (concat
    (lc [[mn [count text-xml]] (slice (list (enumerate G.message-log)) (- height))]
      (amap (, mn count it) (.wrap
        (AttrStr.from-xml (cat text-xml (when (> count 1)
          (.format " [× {}]" count))))
        G.screen-width)))))
  (setv lines (slice lines (- height)))
  (for [[i [mn count astr]] (enumerate lines)]
    (G.T.move (+ i (- G.screen-height height)) 0)
    (.draw astr (if (or
        (> mn G.last-new-message-number)
        (and (= mn G.last-new-message-number)
          (> count G.last-message-count)))
      G.new-msg-highlight
      0))))

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
    (draw-message-log)
    (curses.curs-set 0))
  (when (= G.screen-mode :look)
    (draw-look-legend focus)
    (curses.curs-set 1)
    (apply G.T.move focus-t-coords))
  (G.T.refresh))

(defn render-text-screen [text-xml]
  (curses.curs-set 0)
  (setv w (- G.screen-width G.text-screen-left-margin))
  (setv lines (concat (amap
    (if it (.wrap (AttrStr.from-xml it) w) [(AttrStr)])
    (.split text-xml "\n"))))
  (setv pages [])
  (setv i 0)
  (while True
    (.append pages {"text" (slice lines i (+ i G.screen-height))})
    (when (>= (+ i G.screen-height) (len lines))
      (break))
    ; Remove the bottom line to make room for a "more" prompt.
    (setv (get pages -1 "text") (slice (get pages -1 "text") None -1))
    (setv (get pages -1 "more") True)
    (+= i (+ G.screen-height -1 (- G.text-screen-page-overlap))))
  pages)

(defn draw-text-screen-page [page]
  (G.T.erase)
  (for [[i line] (enumerate (get page "text"))]
    (G.T.move i G.text-screen-left-margin)
    (.draw line))
  (when (.get page "more")
    (G.T.move (len (get page "text")) 0)
    (.draw (AttrStr.from-xml (color-xml "-- more --" G.bg-color G.fg-color)))))

(defn draw-inventory [prompt]
  (setv item-sort-order (list (enumerate [
    (get G.itypes "aoy")
    (rtv-get item.burden.Burden)
    (rtv-get item.clothing.Clothing)
    (rtv-get item.gadget.Gadget)
    (rtv-get item.soda.Soda)
    (rtv-get item.gadget.Battery)])))
  (kwc .sort G.inventory :key (fn [item]
    (setv category (afind-or (instance? (second it) item) item-sort-order))
    (setv category (if category (first category) (len item-sort-order)))
    (,
      category
      (.lower (.format "{}" item))
      (not (.identified? item))
      (when (and (hasattr item "charges") (.identified? item))
        (- item.charges))
      (when (hasattr item "boxed")
        item.boxed)
      item.curse
      item.invlet)))
  (setv names (amap (.format "{:a:most}" it) G.inventory))
  (setv prices (amap (.apparent-price it) G.inventory))
  (setv lines (amap (AttrStr.from-xml it) (+
    [prompt]
    (amap
      (.format "  {} {} {:{}}  {}{:>{}}"
        (. (get G.inventory it) invlet)
        (.xml-symbol (get G.inventory it))
        (get names it)
        (max (map len names))
        (if (zero? it) "$" " ")
        (get prices it)
        (max (amap (len (string it)) prices)))
      (range (len G.inventory)))
    (* ["      ---"] (- G.inventory-limit (len G.inventory))))))
  (setv width (min G.screen-width (inc (max (map len lines)))))
  (for [[n line] (enumerate lines)]
    (G.T.move n 0)
    (.draw (.ljust line width))))

(defn draw-look-legend [p]
  ; In look mode, show a legend describing the creature, item,
  ; and tile under the cursor.
  (setv dunno (unless (seen p) "    ? unseen"))
  (setv lines [
    "At cursor: (press a key to examine)"
    (or dunno (whenn (Creature.at p)
      (.format "  {} {} {:a}" (get look-at-keys :creature) (.xml-symbol it) it)))
    (or dunno
      (when (none? (.visible-item-at G.player p))
        "    ? can't tell")
      (whenn (Item.at p)
        (.format "  {} {} {:a:full}" (get look-at-keys :item) (.xml-symbol it) it)))
    (or dunno
      (.format "  {} {} {:a:full}" (get look-at-keys :tile) (.xml-symbol (Tile.at p)) (Tile.at p)))])
  (setv lines (amap (AttrStr.from-xml (or it "      ---")) lines))
  (assert (= (len lines) G.look-mode-legend-height))
  (setv width (min G.screen-width (inc (max (map len lines)))))
  (for [[n line] (enumerate lines)]
    (G.T.move (- G.screen-height (- (len lines) n)) 0)
    (.draw (.ljust line width))))

(defn describe-tile [pos &optional verbose]
  (setv tile (Tile.at pos))
  (cond
    [(.visible-item-at G.player pos) (do
      (msg "You see here {} {:a:full}." (.xml-symbol (Item.at pos)) (Item.at pos))
      (unless (instance? Floor tile)
        ; This triggers even when 'verbose' is false because
        ; there's an item covering this tile, so the tile type
        ; may not be obvious.
        (msg "There is also {} {:a:full} here." (.xml-symbol tile) tile)))]
    [verbose
      (if (instance? Floor tile)
        (msg :bob "Now the beetle-headed {} is snilching the floor. Wonder what {p:he's} looking for."
          (if (G.player.name.female) "dowdy" "cull"))
        (msg "There is {} {:a:full} here." (.xml-symbol tile) tile))]))
