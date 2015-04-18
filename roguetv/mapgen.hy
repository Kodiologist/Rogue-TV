(require kodhy.macros)

(import
  [random [randint]]
  [libtcodpy :as tcod]
  [heidegger.pos [Pos]]
  heidegger.digger
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [*]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]])

(defn reset-level []
  (setv G.seen-map (amap (* [False] G.map-height) (range G.map-width)))
  (for [t [Tile Item Creature]]
    (.init-omap t G.map-width G.map-height))
  ; Now that we're on a new level, the positions of old
  ; MapObjects are invalid. But that's okay because there's no
  ; way to refer to old MapObjects anymore, either (except for
  ; ones with pos None, like the items in the player's
  ; inventory).

  (setv dugout (kwc heidegger.digger.generate-map
    :width G.map-width :height G.map-height))

  (setv free-floors [])
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (setv p (Pos x y))
      (setv floor? (not (get dugout "map" x y)))
      (when floor?
        (.append free-floors p))
      (mset p (if floor? (Floor) (Wall)))))

  ; Add elevators.
  (setv upelv-pos (Pos (/ G.map-width 2) (/ G.map-height 2)))
  (mset upelv-pos (UpElevator))
  (.remove free-floors upelv-pos)
  (mset (randpop free-floors) (DownElevator))

  ; Add doors.
  (for [p (concat (amap it.doors (get dugout "rooms")))]
    (when (and (instance? Floor (Tile.at p)) (1-in 5))
      (mset p (kwc Door :open-time (pick [3 3.5 4 4.5 5])))
      (.remove free-floors p)))

  ; Add ice, mud, and webs.
  (for [pos free-floors]
    (when (1-in 50)
      (setv mk-tile (pick [
        (fn [] (kwc Ice :max-slip-time 5))
        (fn [] (kwc Mud :max-exit-time 5))
        (fn [] (kwc Web :tear-time (pick [1 2 3 4])))]))
      (setv v (pick [Pos.NORTH Pos.SOUTH]))
      (setv h (pick [Pos.WEST Pos.EAST]))
      (for [dx (range (randint 1 5))]
        (for [dy (range (randint 1 5))]
          (setv p (+ pos (* dx h) (* dy v)))
          (when (and (on-map p) (instance? Floor (Tile.at p)))
            (mset p (mk-tile))
            (.remove free-floors p))))))

  (setv G.time-limit (+ G.current-time (* 5 60)))

  (.move G.player upelv-pos)

  (recompute-fov))
