(require kodhy.macros)

(import
  [random [choice randint]]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat shift ret retf weighted-choice]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Generated Scheduled LevelTimer set-time-limit]]
  [roguetv.map [*]]
  [roguetv.fov [init-fov-map]]
  [roguetv.item [Item Curse]]
  [roguetv.creature [Creature Effect]]
  [roguetv.creature.monster [Snail Spider Nymph Dog Cat]])

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

  (set-time-limit (+ G.current-time (dl-time-limit-cu dl)))

  (setv G.seen-map (amap (* [False] G.map-height) (range G.map-width)))
  (for [t [Tile Item Creature]]
    (.init-omap t G.map-width G.map-height))
  (for [x (list Scheduled.queue)]
    ; Destroy objects that we left behind on the previous level.
    (setv keep (cond
      [(instance? LevelTimer x)
        True]
      [(player? x)
        True]
      [(instance? Effect x)
        ; Effects only apply to the player, so they're going
        ; with the player.
        True]
      [(instance? Creature x)
        False]
      [(instance? Tile x)
        False]
      [(instance? Item x)
        (in x G.inventory)]
      [(instance? Curse x)
        (in x.host-item G.inventory)]
      [True
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
      (mset p (if floor? (Floor) (Wall)) True)))
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
  ; First, extra monsters generated by the player's gear.
  (for [x (filt it.carry-gen-monster (active-inv))]
    (kwc x.carry-gen-monster :pos (shift free-floors)))
  ; Then, regular obstacles.
  (setv Obstacle.dl dl)
  (setv Obstacle.free-floors free-floors)
  (setv Obstacle.door-pos (shuffle (concat (amap
    (amap (- it (Pos 1 1)) it.doors)
    (get dugout "rooms")))))
  (for [o-type (select-obstacles dl)]
    (.f o-type))

  ; Add items.
  ; First, extra items generated by the player's gear.
  (for [x (filt it.carry-gen-item (active-inv))]
    (setv itype (weighted-choice (filt
      (issubclass (second it) x.carry-gen-item)
      (amap
        (, (it.generation-weight dl) it)
        (.values G.itypes)))))
    (kwc itype :pos (shift free-floors)))
  ; Then, regular items.
  (for [[in-chest itype] (select-items dl)]
    (setv p (shift free-floors))
    (kwc itype :pos p)
    (when in-chest
      (mset p (Chest))))

  ; Perhaps add a cat.
  (when (1-in 20)
    (kwc Cat :pos (shift free-floors)))

  (.move G.player upelv-pos)

  (for [item G.inventory]
    (.on-reset-level item)))

(defn select-obstacles [dl]
  (setv weighted-otypes (amap
    (, (it.generation-weight dl) it)
    Obstacle.types))
  (replicate (+ 1 (randgeom (+ 10 (* 2 dl))))
       ; This yields:
       ; level         quantiles
       ;      .025  .25   .5  .75 .975
       ; 1       1    3    8   15   39
       ; 10      1    9   20   40  106
       ; 20      2   14   34   68  179
    (weighted-choice weighted-otypes)))

(defn select-items [dl]
  (setv weighted-itypes (amap
    (, (it.generation-weight dl) it)
    (.values G.itypes)))
  (setv weighted-itypes-chest (amap
    (, (kwc it.generation-weight dl :+in-chest) it)
    (.values G.itypes)))
  (replicate (+ 1 (randgeom (+ 5 dl))) (do
       ; This yields:
       ; level         quantiles
       ;      .025  .25   .5  .75 .975
       ; 1       1    2    4    8   21
       ; 10      1    5   11   21   54
       ; 20      1    8   17   34   91
    (setv in-chest (1-in 8))
    (, in-chest (weighted-choice (if in-chest
      weighted-itypes-chest
      weighted-itypes))))))

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
          (mset p (ClosedDoor))
          (.remove @free-floors p)
          (-= n-to-place 1)
          (unless n-to-place
            (ret)))))))

(defobst O-EmptyChest [Obstacle]
  level-lo 5
  rarity :uncommon
  f (cmeth []
    (mset (shift @free-floors) (Chest))))

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

(defobst O-Slime [MudlikeObstacle]
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Slime)))

(defobst O-Ice [MudlikeObstacle]
  level-lo 1
  level-hi 6
  max-cheb-radius (cmeth []
    (+ 2 (// @dl 4)))
  make-tile (cmeth [] (Ice)))

(defobst O-Webs [MudlikeObstacle]
  level-lo 2
  max-cheb-radius (cmeth []
    (+ 1 (// @dl 4)))
  make-tile (cmeth [] (Web)))

(defobst O-Dogs [Obstacle]
  level-lo 2
  rarity :uncommon
  f (cmeth []
      (setv n-to-place (+ (// @dl 4) (randint 1 3)))
      (for [_ (range n-to-place)]
        (kwc Dog :pos (shift @free-floors)))))

(defcls NormalMonster [Obstacle]
  cr-cls None
  max-to-place 3

  f (cmeth []
      (setv n-to-place (randint 1 @max-to-place))
      (for [_ (range n-to-place)]
        (kwc @cr-cls :pos (shift @free-floors)))))

(defobst O-Snails [NormalMonster]
  cr-cls Snail)

(defobst O-Spiders [NormalMonster]
  level-lo 3
  cr-cls Spider)

(defobst O-Nymph [NormalMonster]
  level-lo 6
  cr-cls Nymph
  max-to-place 1)
