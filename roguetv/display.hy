(require kodhy.macros)

(import
  textwrap
  curses
  [heidegger.pos [Pos]]
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile Floor]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]])

(defn get-color [fg bg]
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
  (G.T.addstr str (get-color color-fg color-bg)))

(defn echo-drawable [d]
  (echo d.char d.color-fg d.color-bg))

(defn ty->py [ty]
  (+ (- (// G.screen-height 2) ty) G.player.pos.y))
(defn tx->px [tx]
  (+ (- tx (// G.screen-width 2)) G.player.pos.x))

(defn draw-map []
  (G.T.move 0 0)
  (for [ty (range (- G.screen-height G.bottom-border))]
    (setv py (ty->py ty))
    (for [tx (range G.screen-width)]
      (setv px (tx->px tx))
      (if
        (and (<= 0 px (dec G.map-width)) (<= 0 py (dec G.map-height))
          (get G.seen-map px py))
        (echo-drawable (let [[p (Pos px py)]] (or
          (Creature.at p)
          (Item.at p)
          (Tile.at p))))
        (echo " " G.fg-color G.unseen-color)))))

(defn draw-status-line []
  (G.T.addstr (- G.screen-height 1 G.message-lines) 0
    (.format "{} {}  DL:{: 2}"
      (.rjust (minsec (max 0 (- (or G.time-limit 0) G.current-time)))
        (len "10:00"))
      (.ljust
        (if G.last-action-duration
         (.format "({})" (show-round G.last-action-duration 2))
          "")
        (len "(1.15)"))
      G.dungeon-level)))

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

(defn draw-inventory [prompt]
  (setv lines (+
    [[None prompt]]
    (amap
      [it (.format "  {} � {}" it.invlet (it.display-name))]
        ; The character � will be replaced with the item's symbol.
      G.inventory)
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

(defn full-redraw []
  (G.T.erase)
  (draw-status-line)
  (draw-bottom-message-log)
  (draw-map)
  (G.T.refresh))

(defn describe-tile [pos &optional verbose]
  (setv tile (Tile.at pos))
  (cond
    [(Item.at pos) (do
      (msgn "You see here {}." (.display-name (Item.at pos)))
      (unless (instance? Floor tile)
        ; This triggers even when 'verbose' is false because
        ; there's an item covering this tile, so the tile type
        ; may not be obvious.
        (msgn "There is also {} here." tile.description)))]
    [verbose
      (if (instance? Floor tile)
        (msg :bob "Now the beetle-headed {} is snilching the floor. Wonder what {p:he's} looking for."
          (if G.player.female "dowdy" "cull"))
        (msgn "There is {} here." tile.description))]))
