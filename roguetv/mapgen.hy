(require [kodhy.macros [lc amap filt afind-or replicate block retf cmeth]])

(import
  random
  [random [choice randint normalvariate]]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [T F unique pairs concat shift ret weighted-choice merge-dicts]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Generated Scheduled LevelTimer set-time-limit]]
  [roguetv.map [*]]
  [roguetv.fov [init-fov-map]]
  [roguetv.item [Item Curse]]
  [roguetv.creature [Creature Effect]]
  [roguetv.creature.monster [Snail Spider Nymph Dog Cat Golem UmberHulk]])

(defn reset-level [&optional [new-seed F]]

  (setv dl G.dungeon-level)

  ; Seed the map generator for this level.
  (setv general-rng-state (random.getstate))
  (if new-seed
    (random.seed)
    (random.seed (repr (, (get G.seeds "map") (int dl)))))
      ; This method of seeding might not be particularly smart.
      ; For now, I'm just covering for Python 3's lack of
      ; random.jumpahead.

  (setv G.map-width (+ 50 (* 4 dl)))
  (setv G.map-height (+ 20 (* 2 dl)))
  (when (and (not-in dl [0 G.max-dungeon-level]) (1-in 10))
    (if (1-in 2)
      (do ; Wide level.
        (+= G.map-width (// G.map-width 2))
        (setv G.map-height 20))
      (do ; Square level.
        (setv d (// (+ G.map-width G.map-height) 2))
        (setv G.map-width d)
        (setv G.map-height d))))

  (set-time-limit (+ G.current-time (dl-time-limit dl)))

  (setv G.seen-map (amap (* [F] G.map-height) (range G.map-width)))
  (for [t [Tile Item Creature]]
    (.init-omap t G.map-width G.map-height))
  (for [x (list Scheduled.queue)]
    ; Destroy objects that we left behind on the previous level.
    (setv keep (cond
      [(instance? LevelTimer x)
        T]
      [(player? x)
        T]
      [(instance? Effect x)
        ; Effects only apply to the player, so they're going
        ; with the player.
        T]
      [(instance? Creature x)
        F]
      [(instance? Tile x)
        F]
      [(instance? Item x)
        (in x G.inventory)]
      [(instance? Curse x)
        (in x.host-item G.inventory)]
      [T
        (raise (ValueError (.format "Weird thing in Schedule.queue: {!r}" x)))]))
    (unless keep
      (.destroy x)))
  (init-fov-map Tile.omap)
  ; Now that we're on a new level, the positions of old
  ; MapObjects are invalid. But that's okay because there's no
  ; way to refer to old MapObjects anymore, either (except for
  ; ones with pos None, like the items in the player's
  ; inventory).

  ; We have heidegger.digger generate a slightly larger map
  ; because it always leaves an outermost border of wall square,
  ; which in our case would be redundant with the map border.
  (setv dugout (heidegger.digger.generate-map
    :room-width [2 8] :room-height [2 5]
    :corridor-length [3 10]
    :dug-fraction (ilogit (normalvariate (logit .2) .25))
      ; Quantile rank  .01 .25 .50 .75 .99
      ; Quantile       .12 .17 .20 .23 .31
    :width (+ G.map-width 2) :height (+ G.map-height 2)))
  
  (setv free-floors [])
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (setv p (Pos x y))
      (setv floor? (not (get dugout "map" (inc x) (inc y))))
      (when floor?
        (.append free-floors p))
      (mset p (if floor? (Floor) (Wall)) T)))
  (setv free-floors (shuffle free-floors))

  ; Add elevators.
  ;
  ; The up elevator, where the player starts, is always put in
  ; the center, which heidegger.digger always leaves as floor.
  (setv upelv-pos (Pos (/ G.map-width 2) (/ G.map-height 2)))
  (assert (instance? Floor (Tile.at upelv-pos)))
  (mset upelv-pos (UpElevator))
  (.remove free-floors upelv-pos)
  ; The down elevator is placed randomly.
  (mset (shift free-floors) (DownElevator))
  ; Possibly also place an express elevator.
  (when (and (< dl (dec G.max-dungeon-level)) (1-in 10))
    (mset (shift free-floors)
      (if (and (= dl 0) (1-in 10))
        (Hellevator)
        (ExpressElevator :depth (randint
          G.express-elevator-min-depth
          G.express-elevator-max-depth)))))

  ; Generate the Amulet if the player is on the last level.
  (when (= dl G.max-dungeon-level)
    ((get G.itypes "aoy") :pos (shift free-floors)))

  ; Add obstacles.
  (setv Obstacle.dl dl)
  (setv Obstacle.free-floors free-floors)
  (setv Obstacle.door-pos (shuffle (concat (amap
    (amap (- it (Pos 1 1)) it.doors)
    (get dugout "rooms")))))
  (setv Obstacle.golem-pos [])
  (for [o-type (select-obstacles dl)]
    (.f o-type))

  ; Add benefits.
  (setv Benefit.dl dl)
  (setv Benefit.free-floors free-floors)
  (for [b-type (select-benefits dl)]
    (.f b-type))

  ; Add items.
  (for [[in-chest itype1 itype2] (select-items dl)]
    (setv p (shift free-floors))
    (setv itype (if (.unique-and-already-generated itype1) itype2 itype1))
    (itype :pos p)
    (when in-chest
      (mset p (Chest))))

  ; Perhaps add a cat.
  (when (1-in 20)
    (Cat :pos (shift free-floors)))

  ; Switch back to the general seed. Now we can add things to the
  ; map that depend on game state.
  (random.setstate general-rng-state)
  ; Re-shuffle free-floors so it can be influenced by the general
  ; RNG state.
  (setv free-floors (shuffle free-floors))

  ; Make extra monsters generated by the player's gear.
  (for [x (filt it.carry-gen-monster (active-inv))]
    (x.carry-gen-monster :pos (shift free-floors)))

  ; Make extra items generated by the player's gear.
  (for [x (filt it.carry-gen-item (active-inv))]
    (setv itype (weighted-choice (filt
      (issubclass (second it) x.carry-gen-item)
      (amap
        (, (it.generation-weight dl) it)
        (values-sorted-by-keys G.itypes)))))
    (itype :pos (shift free-floors)))

  ; Finishing touches.
  (.move G.player upelv-pos)
  (for [item G.inventory]
    (.on-reset-level item)))

(defn gen-count [dl mean-base mean-dl sd-base sd-dl minimum]
  (int (round (max minimum (normalvariate
    (+ mean-base (* dl mean-dl))
    (+ sd-base (* dl sd-dl)))))))
(setv gen-count-params (dict
  :obstacles (dict
    :mean_base 8 :mean_dl 2
    :sd_base 3 :sd_dl 1
    :minimum 1)
  :benefits (dict
    :mean_base 0 :mean_dl .5
    :sd_base 1 :sd_dl .25
    :minimum 0)
  :items (dict
    :mean_base 4 :mean_dl .5
    :sd_base 2 :sd_dl .25
    :minimum 1)))
(defn gen-count-for [dl thingtype]
  (gen-count :dl dl #** (get gen-count-params thingtype)))

(defn select-obstacles [dl]
  (setv weighted-otypes (amap
    (, (it.generation-weight dl) it)
    Obstacle.types))
  (replicate (gen-count-for dl "obstacles")
    (weighted-choice weighted-otypes)))

(defn select-benefits [dl]
  (setv weighted-btypes (amap
    (, (it.generation-weight dl) it)
    Benefit.types))
  (replicate (gen-count-for dl "benefits")
    (weighted-choice weighted-btypes)))

(defn select-items [dl]
  (replicate (gen-count-for dl "items")
    (setv in-chest (1-in 8))
    (setv itype1 (weighted-choice (amap
      (, (it.generation-weight dl :in-chest in-chest) it)
      (values-sorted-by-keys G.itypes))))
    (setv itype2 (when itype1.unique
      ; Pick a non-unique substitute in case item1 has already
      ; been generated. We always do this weighted-choice, even
      ; if itype1 is new, so that which unique items exist doesn't
      ; affect the evolution of the map seed.
      (weighted-choice (amap
        (, (it.generation-weight dl :in-chest in-chest) it)
        (filt (not it.unique) (values-sorted-by-keys G.itypes))))))
    (, in-chest itype1 itype2)))

(defclass Obstacle [Generated] [
  types []])

(defmacro defobst [name inherit &rest body]
  `(do
    (defclass ~name ~inherit ~@body)
    ; Add this obstacle type to the list.
    (.append Obstacle.types ~name)))

(defobst O-Doors [Obstacle] [
  f (cmeth [] (block
      (setv n-to-place (inc (// @dl 3)))
      (for [p @door-pos]
        (when (in p (list @free-floors))
          (mset p (ClosedDoor))
          (.remove @free-floors p)
          (-= n-to-place 1)
          (unless n-to-place
            (ret))))))])

(defobst O-EmptyChest [Obstacle] [
  level-lo 9
  rarity :uncommon
  f (cmeth []
    (mset (shift @free-floors) (Chest)))])

(defclass MudlikeObstacle [Obstacle] [
  max-cheb-radius None
  make-tile None

  make-tiles (cmeth [ps]
    (for [p ps]
      (mset p (@make-tile))))

  f (cmeth []
      (setv mcr (@max-cheb-radius))
      (setv n-to-place (randint
        (round (* .25 (** (* 2 mcr) 2)))
        (round (* .75 (** (* 2 mcr) 2)))))
      (setv start (shift @free-floors))
      (setv occupied [start])
      (block :done (while n-to-place (block :again
        (for [op occupied]
          (for [d (shuffle Pos.ORTHS)]
            (setv p (+ op d))
            (when (and
                (<= (dist-cheb start p) mcr)
                (in p @free-floors)
                (not-in p occupied))
              (.insert occupied (randint 0 (len occupied)) p)
              (.remove @free-floors p)
              (-= n-to-place 1)
              (retf (if n-to-place :again :done)))))
        ; We couldn't find anywhere to place the remaining tiles.
        ; So, just quit the outer loop.
        (break))))
      (@make-tiles occupied))])

(defobst O-Slime [MudlikeObstacle] [
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Slime))])

(defobst O-Ice [MudlikeObstacle] [
  level-lo 1
  level-hi 6
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Ice))])

(defobst O-Webs [MudlikeObstacle] [
  level-lo 2
  max-cheb-radius (cmeth []
    (+ 1 (// @dl 4)))
  make-tile (cmeth [] (Web))])

(defobst O-PusherTiles [MudlikeObstacle] [
  level-lo 7
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tiles (cmeth [ps]
    (setv open [])
    (setv closed (list Pos.ORTHS))
    (for [p ps]
      (for [d (list closed)]
        (when (and (not-in (+ d p) ps)
            (not (. (mget (+ d p)) blocks-movement)))
          (.append open d)
          (.remove closed d))))
    (setv tile-type (get PusherTile.children
      (choice (or open Pos.ORTHS))))
    (for [p ps]
      (mset p (tile-type))))])

(defobst O-StasisTraps [Obstacle] [
  level-lo 4
  off-time-table (pairs
    0  (seconds 10)
    1  (seconds 8)
    2  (seconds 5)
    6  (seconds 4)
    8  (seconds 3)
    10 (seconds 2)
    12 (seconds 2)
    15 (seconds 1)
    18 (seconds .5))
  off-time (cmeth []
    (choice (lc [[min-dl span] @off-time-table]
      (>= @dl min-dl)
      span)))
  on-time (cmeth []
    (seconds (randint 2 (+ 2 @dl))))
  f (cmeth []
    (setv start (get @free-floors 0))
    ; Send two orthogonal line segments out from the start until
    ; each end hits a position that isn't a free floor. Place
    ; stasis traps along the shorter line segment.
    (setv spokes (dict
      (amap
        (, it (do
           (setv spoke [start])
           (while (in (+ (get spoke -1) it) @free-floors)
             (.append spoke (+ (get spoke -1) it)))
           spoke))
        Pos.ORTHS)))
    (setv line (min :key len (shuffle [
      (+ (get spokes Pos.WEST) (get spokes Pos.EAST))
      (+ (get spokes Pos.NORTH) (get spokes Pos.SOUTH))])))
    (setv off-time (@off-time))
    (setv on-time (@on-time))
    (for [p (unique line)]
      (.remove @free-floors p)
      (mset p (StasisTrap off-time on-time))))])

(defobst O-SpookyTotems [Obstacle] [
  level-lo 11
  f (cmeth []
    (mset (shift @free-floors) (SpookyTotem)))])

(defobst O-Dogs [Obstacle] [
  level-lo 2
  rarity :uncommon
  f (cmeth []
      (setv n-to-place (+ (// @dl 4) (randint 1 3)))
      (for [_ (range n-to-place)]
        (Dog :pos (shift @free-floors))))])

(defclass NormalMonster [Obstacle] [
  cr-cls None
  max-to-place 3

  f (cmeth []
    (setv n-to-place (randint 1 @max-to-place))
    (for [_ (range n-to-place)]
      (@cr-cls :pos (shift @free-floors))))])

(defobst O-Snails [NormalMonster] [
  cr-cls Snail])

(defobst O-Golem [Obstacle] [
  level-lo 2

  max-to-place 3

  f (cmeth []
    (setv n-to-place (randint 1 @max-to-place))
    (for [p (list @free-floors)]
      (when (afind-or (or (= it.x p.x) (= it.y p.y)) @golem-pos)
        ; Never place a golem on a horizontal or vertical line
        ; with another golem. That could make it impossible for
        ; the player to get through a corridor.
        (continue))
      (Golem :pos p)
      (.append @golem-pos p)
      (.remove @free-floors p)
      (-= n-to-place 1)
      (unless n-to-place
        (break))))])

(defobst O-Spiders [NormalMonster] [
  level-lo 4
  cr-cls Spider])

(defobst O-Nymph [NormalMonster] [
  level-lo 7
  cr-cls Nymph
  max-to-place 1])

(defclass Benefit [Generated] [
  types []])

(defmacro defben [name inherit &rest body]
  `(do
    (defclass ~name ~inherit ~@body)
    ; Add this benefit type to the list.
    (.append Benefit.types ~name)))

(defben B-DoublingMachine [Benefit] [
  f (cmeth []
    (mset (shift @free-floors) (DoublingMachine)))])

(defben B-UmberHulk [Benefit] [
  level-lo 2
  f (cmeth []
    (UmberHulk :pos (shift @free-floors)))])
