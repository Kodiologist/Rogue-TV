(require kodhy.macros)

(import
  [random [choice randint]]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat shift ret retf]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Generated]]
  [roguetv.map [*]]
  [roguetv.fov [init-fov-map]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]]
  [roguetv.creature.monster [Snail Dog Cat]])

(defn reset-level []
  (setv dl G.dungeon-level)

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

  (setv G.time-limit (+ G.current-time (int (* 60
    (+ 3 (/ dl 2))))))

  (setv G.seen-map (amap (* [False] G.map-height) (range G.map-width)))
  (for [t [Tile Item Creature]]
    (.init-omap t G.map-width G.map-height))
  (setv Creature.extant [G.player])
  (init-fov-map Tile.omap)
  ; Now that we're on a new level, the positions of old
  ; MapObjects are invalid. But that's okay because there's no
  ; way to refer to old MapObjects anymore, either (except for
  ; ones with pos None, like the items in the player's
  ; inventory).

  ; We have heidegger.digger generate a slightly larger map
  ; because it always leaves an outermost border of wall square,
  ; which in our case would be redundant with the map border.
  (setv dugout (kwc heidegger.digger.generate-map
    :room-width [2 8] :room-height [2 5]
    :corridor-length [3 10]
    :dug-fraction .2
    :width (+ G.map-width 2) :height (+ G.map-height 2)))
  
  (setv free-floors [])
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (setv p (Pos x y))
      (setv floor? (not (get dugout "map" (inc x) (inc y))))
      (when floor?
        (.append free-floors p))
      (mset p (if floor? (Floor) (Wall)) False)))
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

  ; Generate the Amulet if the player is on the last level.
  (when (= dl G.max-dungeon-level)
    (kwc (get G.itypes "aoy") :pos (shift free-floors)))

  ; Add obstacles.
  (setv n-obstacles (randint (+ dl 3) (* 2 (+ dl 3))))
    ; So 3 to 6 obstacles on level 0,
    ; and 12 to 24 on level 9.
  (setv Obstacle.dl dl)
  (setv Obstacle.free-floors free-floors)
  (setv Obstacle.door-pos (shuffle (concat (amap
    (amap (- it (Pos 1 1)) it.doors)
    (get dugout "rooms")))))
  (for [_ (range n-obstacles)]
    (setv o-type (weighted-choice (amap
      (, (it.generation-weight dl) it)
      Obstacle.types)))
    (.f o-type))

  ; Add items.
  (setv n-items (inc (randpois (+ 3 (/ dl 5)))))
    ; This yields:
    ; level         quantiles
    ;         .025  .25 .5 .75 .975
    ; 0        1     3   4   5   8
    ; 9        2     4   6   7   11
  (for [_ (range n-items)]
    (setv itype (weighted-choice (amap
      (, (it.generation-weight dl) it)
      (.values G.itypes))))
    (kwc itype :pos (shift free-floors)))

  ; Perhaps add a cat.
  (when (1-in 20)
    (kwc Cat :pos (shift free-floors)))

  (.move G.player upelv-pos)

  (for [item G.inventory]
    (.on-reset-level item)))

(defcls Obstacle [Generated]
  types [])

(defmacro defobst [name inherit &rest body]
  `(do
    (defcls ~name ~inherit ~@body)
    ; Add this obstacle type to the list.
    (.append Obstacle.types ~name)))

(defobst O-Doors [Obstacle]
  f (cmeth [] (block
      (setv n-to-place (inc (// @dl 3)))
      (for [p @door-pos]
        (when (in p (list @free-floors))
          (mset p (Door))
          (.remove @free-floors p)
          (-= n-to-place 1)
          (unless n-to-place
            (ret)))))))

(defcls MudlikeObstacle [Obstacle]
  max-cheb-radius None
  make-tile None

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
      (for [p occupied]
        (mset p (@make-tile)))))

(defobst O-Webs [MudlikeObstacle]
  level-lo 0
  level-hi 5
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Web)))

(defobst O-Ice [MudlikeObstacle]
  level-lo 1
  level-hi 6
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Ice)))

(defobst O-Slime [MudlikeObstacle]
  level-lo 3
  max-cheb-radius (cmeth []
    (+ 1 (// @dl 4)))
  make-tile (cmeth [] (Slime)))

(defobst O-Dogs [Obstacle]
  level-lo 2
  rarity :uncommon
  f (cmeth []
      (setv n-to-place (+ (// @dl 4) (randint 1 3)))
      (for [_ (range n-to-place)]
        (kwc Dog :pos (shift @free-floors)))))

(defobst O-Snails [Obstacle]
  f (cmeth []
      (setv n-to-place (randint (inc @dl) (* 2 (inc @dl))))
      (for [_ (range n-to-place)]
        (kwc Snail :pos (shift @free-floors)))))
