(require kodhy.macros)

(import
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

  (setv upelv-pos (Pos (/ G.map-width 2) (/ G.map-height 2)))
  (mset upelv-pos (UpElevator))
  (.remove free-floors upelv-pos)
  (mset (randpop free-floors) (DownElevator))

  (for [p (concat (amap it.doors (get dugout "rooms")))]
    (when (and (instance? Floor (Tile.at p)) (1-in 5))
      (mset p (kwc Door :open-time (pick [3 3.5 4 4.5 5])))
      (.remove free-floors p)))

  (setv G.time-limit (+ G.current-time (* 5 60)))

  (.move G.player upelv-pos))
