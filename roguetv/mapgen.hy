(require kodhy.macros)

(import
  [libtcodpy :as tcod]
  [heidegger.pos [Pos]]
  heidegger.digger
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [*]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]])

(defn reset-level []
  (setv dugout (kwc heidegger.digger.generate-map
    :width G.map-width :height G.map-height))

  (setv free-floors [])
  (for [x (range G.map-width)]
    (for [y (range G.map-height)]
      (setv p (Pos x y))
      (setv floor? (not (get dugout "map" x y)))
      (when floor?
        (.append free-floors p))
      (mset p (if floor? (Floor) (Wall)))
      (tcod.map-set-properties G.fov-map x y floor? floor?)))

  (setv upelv-pos (Pos (/ G.map-width 2) (/ G.map-height 2)))
  (mset upelv-pos (UpElevator))
  (.remove free-floors upelv-pos)
  (mset (randpop free-floors) (DownElevator))

  (setv G.seen-map (amap (* [False] G.map-height) (range G.map-width)))
  (.init-omap Item G.map-width G.map-height)
  (.init-omap Creature G.map-width G.map-height)
  ; Now that we're on a new level, the positions of old
  ; MapObjects are invalid. But that's okay because there's no
  ; way to refer to old MapObjects anymore, either (except for
  ; ones with pos None, like the items in the player's
  ; inventory).

  (setv G.time-limit (+ G.current-time (* 5 60)))

  (.move G.player upelv-pos))
