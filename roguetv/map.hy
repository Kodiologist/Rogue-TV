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

  [use-tile (fn [self]
    ; The player has used the command :use-tile on top of this
    ; tile. Do whatever is necessary and return the time
    ; taken.
    ;
    ; The default implementaton does nothing.
    (msgn "There's nothing special you can do at this tile.")
    0)]])

(defclass Floor [Tile] [
  [char "."]])

(defclass Wall [Tile] [
  [description "a wall"]
  [char "#"]
  [color-bg G.fg-color]
  [blocks-movement True]])

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

(defn mset [pos tile]
  (kwc .move tile pos :+clobber))

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
