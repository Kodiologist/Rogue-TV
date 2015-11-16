(require kodhy.macros)

(import
  random
  gzip
  jsonpickle
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [MapObject Scheduled]]
  [roguetv.map [Tile mset tile-save-shorthand]]
  [roguetv.fov [init-fov-map]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]]
  [roguetv.creature.player [Player]])

(defn write-save-file [path]
  (setv x {})

  (setv (get x "random_state") (random.getstate))

  (setv (get G.dates "saved") (real-timestamp))
  (setv (get x "G") (dict (amap
    (, it (getattr G it))
    G.globals-to-save)))

  (setv (get x "Player") {
    "name" Player.name})

  (setv (get x "item_appearances") (dict (fmap
    (, it (. (get G.itypes it) appearance))
    (. (get G.itypes it) appearance)
    (.keys G.itypes))))

  (setv (get x "omaps") (dict (amap
    (, it.__name__
      (filt (not (none? it)) (concat it.omap)))
    [Item Creature])))
  (setv (get x "map") (list (reversed (amap (list it) (apply zip (amap
    (amap (if (and (in (type it) tile-save-shorthand) (= (.keys it.__dict__) ["pos"]))
      (get tile-save-shorthand (type it))
      it) it)
    Tile.omap))))))

  (setv (get x "Scheduled.queue") Scheduled.queue)

  (with [[o (gzip.open path "wb")]]
    (o.write (kwc jsonpickle.encode x :+warn :+keys))))

(defn load-from-save-file [path]
  (with [[o (gzip.open path "rb")]]
    (setv x (kwc jsonpickle.decode (.read o) :+keys)))

  (for ([k v] (.items (get x "G")))
    (setattr G k v))

  (for ([k v] (.items (get x "Player")))
    (setattr Player k v))

  (for ([k v] (.items (get x "item_appearances")))
    (.set-appearance (get G.itypes k) v))

  ; A bit of extra explicit initialization is necessary here
  ; because the omaps, FOV map, and G.player are redundant with
  ; MapObject fields.
  (for [cls [Tile Item Creature]]
    (.init-omap cls G.map-width G.map-height))
  (setv inverted-tile-save-shorthand (dict
    (lc [[k v] (.items tile-save-shorthand)] (, v k))))
  (for [[yt row] (enumerate (reversed (get x "map")))]
    (for [[xt t] (enumerate row)]
      (if (string? t)
        ((get inverted-tile-save-shorthand t) (Pos xt yt))
        (mset (Pos xt yt) t False))))
  (for [o (+ (get x "omaps" "Item") (get x "omaps" "Creature"))]
    (MapObject.__init__ o o.pos))
  (setv Scheduled.queue (get x "Scheduled.queue"))
  (init-fov-map Tile.omap)

  (random.setstate (get x "random_state"))

  (setv (get G.dates "loaded") (real-timestamp)))

(defn transpose [l]
  (amap (list it) (apply zip l)))
