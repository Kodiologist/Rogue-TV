(require kodhy.macros)

(import
  random
  [datetime [datetime]]
  gzip
  jsonpickle
  [kodhy.util [concat]]
  [roguetv.globals :as G]
  [roguetv.types [MapObject Scheduled]]
  [roguetv.map [Tile mset]]
  [roguetv.fov [init-fov-map]]
  [roguetv.item [Item]]
  [roguetv.creature [Creature]]
  [roguetv.creature.player [Player]])

(defn write-save-file [path]
  (setv x {})

  (setv (get x "random_state") (random.getstate))

  (setv (get G.dates "saved") (.isoformat (datetime.utcnow)))
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
    [Tile Item Creature])))

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
  (for [t [Tile Item Creature]]
    (.init-omap t G.map-width G.map-height))
  (for [o (get x "omaps" "Tile")]
    (mset o.pos o False))
  (for [o (+ (get x "omaps" "Item") (get x "omaps" "Creature"))]
    (MapObject.__init__ o o.pos))
  (setv Scheduled.queue (get x "Scheduled.queue"))
  (init-fov-map Tile.omap)

  (random.setstate (get x "random_state"))

  (setv (get G.dates "loaded") (.isoformat (datetime.utcnow))))
