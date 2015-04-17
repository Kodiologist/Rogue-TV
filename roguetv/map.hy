(require kodhy.macros roguetv.macros)

(import
  [libtcodpy :as tcod]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [y-or-n]]
  [roguetv.types [Drawable MapObject]])

(defclass Tile [Drawable MapObject] [
  [description None]
  [blocks-movement False]
  [blocks-sight False]

  [use-tile (fn [self]
    ; The player has used the command :use-tile on top of this
    ; tile. Do whatever is necessary and return the time
    ; taken.
    ;
    ; The default implementaton does nothing.
    (msgn "There's nothing special you can do at this tile.")
    0)]

  [step-onto (fn [self cr]
    ; A creature has tried to step onto this tile (and
    ; blocks-movement is false). Return True if they can.
    True)]])

(defclass Floor [Tile] [
  [char "."]])

(defclass Wall [Tile] [
  [description "a wall"]
  [char "#"]
  [color-bg G.fg-color]
  [blocks-movement True]
  [blocks-sight True]])

(defclass Elevator [Tile] [])

(defclass UpElevator [Elevator] [
  [description "an elevator going up"]
  [char "<"]

  [use-tile (fn [self]
    (msg :tara "It looks like {p:name} is thinking of taking the elevator back up. If {p:he} {p:v:does}, {p:he} may keep all {p:his} currently held winnings, but {p:he} will lose whatever vast riches {p:he} might've gained here or in lower levels of the Dungeons of Doom, and {p:his} game of Rogue TV will be over! How will {p:he} decide?")
    (when (y-or-n "Take the elevator up?" :+require-uppercase)
      (setv G.endgame :used-up-elevator))
    0)]])

(defclass DownElevator [Elevator] [
  [description "an elevator going down"]
  [char ">"]

  [use-tile (fn [self]
    (+= G.dungeon-level 1)
    (rtv mapgen.reset-level)
    (recompute-fov)
    (msg :tara "And {p:he's} on to the next level.")
    0)]])

(defclass Door [Tile] [
  [description "a closed door"]
  [char "+"]
  [blocks-sight True]

  [__init__ (fn [self &optional open-time]
    (.__init__ (super Door self))
    (set-self open-time)
    None)]

  [step-onto (fn [self cr]
    (when (is cr G.player)
      (msgn "You open the old door after a struggle."))
    (cr.take-time self.open-time)
    (mset self.pos (Floor))
    (recompute-fov)
    False)]])

(defn mset [pos tile]
  (kwc .move tile pos :+clobber)
  (tcod.map-set-properties G.fov-map pos.x pos.y
    (not tile.blocks-sight) (not tile.blocks-movement)))

(defn on-map [pos]
  (and (<= 0 pos.x (dec G.map-width)) (<= 0 pos.y (dec G.map-height))))

(defn recompute-fov []
  (kwc tcod.map-compute-fov G.fov-map
    G.player.pos.x G.player.pos.y
    :algo tcod.FOV-BASIC)
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (when (tcod.map-is-in-fov G.fov-map x y)
        (setv (get G.seen-map x y) True)))))

(defn room-for? [mo-class pos]
  (and
    (on-map pos)
    (not (. (Tile.at pos) blocks-movement))
    (not (.at mo-class pos))))
