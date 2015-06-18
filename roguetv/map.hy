(require kodhy.macros roguetv.macros)

(import
  [random [randint]]
  [libtcodpy :as tcod]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [y-or-n]]
  [roguetv.types [Drawable MapObject]])

(defclass Tile [Drawable MapObject] [
  [description None]
  [blocks-movement False]
  [blocks-sight False]
  [unpleasant False]
    ; `unpleasant` is a flag meaning that monsters tend not to
    ; want to be in this kind of tile.

  [use-tile (fn [self cr]
    ; A creature has tried to do something with this tile. (For
    ; the player, that would be using the command :use-tile.)
    ;
    ; The default implementaton does nothing.
    (msgp cr "There's nothing special you can do at this tile."))]

  [bump-into (fn [self cr]
    ; A creature has tried to step onto this tile. Return
    ; False to halt further processing of the step (in particular,
    ; the creature is not moved), and True to continue.
    True)]

  [after-step-onto (fn [self cr p-from]
    ; A creature has just finished stepping onto this tile.
    None)]

  [step-out-of (fn [self cr p-to]
    ; A creature is stepping out of this tile.
    None)]])

(defn mset [pos tile]
  (kwc .move tile pos :+clobber)
  (when (or
      (!= (tcod.map-is-transparent G.fov-map pos.x pos.y)
        (not tile.blocks-sight))
      (!= tcod.map-is-walkable  G.fov-map pos.x pos.y)
        (not tile.blocks-movement))
    (soil-fov))
  (tcod.map-set-properties G.fov-map pos.x pos.y
    (not tile.blocks-sight) (not tile.blocks-movement)))

(defn on-map [pos]
  (and (<= 0 pos.x (dec G.map-width)) (<= 0 pos.y (dec G.map-height))))

(defn room-for? [mo-class pos]
  (and
    (on-map pos)
    (not (. (Tile.at pos) blocks-movement))
    (not (.at mo-class pos))))

(defn ray-taxi [
    start     ; Pos
    direction ; Pos
    length    ; int
    &optional
    [include-off-map False]]
  ; Produces a list of Pos. `direction` should be in Pos.DIR8.
  ; If `direction` is orthogonal, you get `length` elements.
  ; Otherwise, you get `length` // 2 elements.
  (setv l [start])
  (for [_ (range (dec (if (in direction Pos.DIAGS) (// length 2) length)))]
    (setv p (+ (get l -1) direction))
    (unless (or include-off-map (on-map p))
      (break))
    (.append l p))
  l)

(defn disc-taxi [
    center  ; Pos
    radius] ; int
  ; Produces a list of Pos. In taxicab geometry, a disc is a square
  ; sitting on one of its vertices.
  (list-comp (+ center (Pos dx dy))
    [
      dx (seq (- radius) radius)
      dy (seq (- (abs dx) radius) (- radius (abs dx)))]
    (on-map (+ center (Pos dx dy)))))

(defclass Floor [Tile] [
  [description "ordinary floor"]
  [char "."]])

(defclass Wall [Tile] [
  [description "a wall"]
  [char "#"]
  [color-bg G.fg-color]
  [blocks-movement True]
  [blocks-sight True]])

(defclass Elevator [Tile] [
  [color-fg :white]
  [color-bg :dark-green]])

(defclass UpElevator [Elevator] [
  [description "an elevator going up"]
  [char "<"]

  [use-tile (fn [self cr]
    (when (player? cr)
      (msg :tara "It looks like {p:the} is thinking of taking the elevator back up. If {p:he} {p:v:does}, {p:he} may keep all {p:his} currently held winnings, but {p:he} will lose whatever vast riches {p:he} might've gained here or in lower levels of the Dungeons of Doom, and {p:his} game of Rogue TV will be over! How will {p:he} decide?")
      (when (y-or-n "Take the elevator up?" :+require-uppercase)
        (setv G.endgame :used-up-elevator))))]])

(defclass DownElevator [Elevator] [
  [description "an elevator going down"]
  [char ">"]

  [use-tile (fn [self cr]
    (when (player? cr)
      (+= G.dungeon-level 1)
      (rtv mapgen.reset-level)
      (msg :tara "And {p:he's} on to the next level. This one spans {} by {} squares."
        G.map-width G.map-height)))]])

(defclass Door [Tile] [
  [description "a closed door"]
  [char "+"]
  [color-fg :brown]
  [blocks-movement True]
  [blocks-sight True]

  [__init__ (fn [self open-time]
    (.__init__ (super Door self))
    (set-self open-time)
    None)]

  [bump-into (fn [self cr] (block
    (unless cr.can-open-doors
      (ret True))
    (msgp cr "You open the old door after a struggle.")
    (cr.take-time self.open-time)
    (mset self.pos (Floor))
    False))]])

(defclass Mud [Tile] [
  [description "a pit full of mud"]
  [char "}"]
  [color-fg :white]
  [color-bg :brown]

  [unpleasant True]

  [__init__ (fn [self max-exit-time]
    (.__init__ (super Mud self))
    (set-self max-exit-time)
    None)]

  [step-out-of (fn [self cr p-to]
    (cr.take-time (randint 1 self.max-exit-time)))]])

(defclass Web [Tile] [
  [description "a spiderweb"]
  [char "%"]
  [color-fg :dark-blue]

  [unpleasant True]

  [__init__ (fn [self tear-time]
    (.__init__ (super Web self))
    (set-self tear-time)
    None)]

  [step-out-of (fn [self cr p-to]
    (msgp cr "You tear through the web.")
    (cr.take-time self.tear-time)
    (mset self.pos (Floor)))]])

(defclass Ice [Tile] [
  [description "a patch of ice"]
  [char ":"]
  [color-fg :white]
  [color-bg :pale-azure]

  [unpleasant True]

  [__init__ (fn [self max-slip-time]
    (.__init__ (super Ice self))
    (set-self max-slip-time)
    None)]

  [after-step-onto (fn [self cr p-from]
    ; Set up the creature to slip.
    (setv cr.ice-slip-towards (- self.pos p-from))
    (setv cr.ice-slip-time (randint 1 self.max-slip-time)))]

  [step-out-of (fn [self cr p-to]
    ; If the creature moves in the same direction it moved to
    ; get here, it doesn't slip.
    (when (= (- p-to self.pos) cr.ice-slip-towards)
      (cr.reset-ice-slipping)))]])
