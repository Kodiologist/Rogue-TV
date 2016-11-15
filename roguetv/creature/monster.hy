(require [kodhy.macros [amap filt afind-or whenn block λ meth]] [roguetv.macros [*]])

(import
  [random [choice]]
  pypaths.astar
  [heidegger.pos [Pos]]
  [kodhy.util [ret weighted-choice maxes]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Scheduled]]
  [roguetv.map [Tile Wall Floor Slime Web room-for? mget mset on-map disc-taxi in-los?]]
  [roguetv.item [Item drop-pos]]
  [roguetv.creature [Creature Stink]])

(defclass Monster [Creature] [
  ; A class for all non-player creatures.

  walk-to (fn [self p-to]
    (unless (.walk-to (super Monster self) p-to)
      (raise (ValueError (.format "{} tried to walk where it couldn't: {}" self p-to)))))

  player-repulsive? (fn [self]
    (or
      (.get-effect G.player Stink)
      (afind-or (instance? it.carry-repel-monster self)
        (filt it.carry-repel-monster (active-inv)))))

  flee-from-player (fn [self] (block
    ; If the player is repulsive to us and we're in range, try to
    ; run away (not very intelligently), and return True.
    ; Otherwise, return False.
    (unless (and
        (.player-repulsive? self)
        (<= (dist-taxi self.pos G.player.pos) G.repulsed-from-player-range))
      (ret False))
    (setv neighbors (sorted
      (shuffle (clear-neighbors self.pos))
      :key (λ (,
        (- (/ (dist-taxi it G.player.pos) (dist-taxi it self.pos)))
        (dist-taxi it self.pos)))))
    (if (and neighbors (>
        (dist-taxi (first neighbors) G.player.pos)
        (dist-taxi self.pos G.player.pos)))
      (.walk-to self (first neighbors))
      (.wait self))
    True))])

(defn extant-monsters []
  (filt (instance? Monster it) Scheduled.queue))

(defn clear-neighbors [pos]
  (filt (room-for? Creature it)
    (amap (+ pos it) Pos.DIR8)))

(defn wander [cr &optional [okay? (λ True)]] (block
  ; Try to step in a random direction. (Diagonal moves are half
  ; as likely as orthogonal moves.) Return a boolean indicating
  ; whether we succeeded.
  (setv neighbors (filt (okay? it) (clear-neighbors cr.pos)))
  (unless neighbors
    (ret False))
  (setv p-to (weighted-choice (amap
    (, (/ 1 (dist-taxi cr.pos it)) it)
    neighbors)))
  (.walk-to cr p-to)
  True))

(defn find-path [p-from p-to &optional [max-cost (int 1e6)]] (block
  (when (= p-from p-to)
    (ret [p-to]))
  (setv searcher (pypaths.astar.pathfinder
    :neighbors (fn [p] (+ (clear-neighbors p)
      (if (adjacent? p p-to) [p-to] [])))
    :distance dist-euclid
      ; This is the heuristic. Because the 2-norm ≤ the 1-norm,
      ; Euclidean distance is an admissible heuristic for taxicab
      ; geometry.
    :cost dist-taxi))
  (cut (second (searcher p-from p-to max-cost)) 1)))

(defn find-path-thru-creatures [p-from p-to &optional [max-cost (int 1e6)]]
  (setv searcher (pypaths.astar.pathfinder
    :neighbors (fn [p] (+
       (filt (and (on-map it) (not (. (Tile.at it) blocks-movement)))
         (amap (+ p it) Pos.DIR8))
       (if (adjacent? p p-to) [p-to] [])))
    :distance dist-euclid
      ; This is the heuristic. Because the 2-norm ≤ the 1-norm,
      ; Euclidean distance is an admissible heuristic for taxicab
      ; geometry.
    :cost (fn [p1 p2]
      ; Prefer paths that don't go through creatures.
      (when (or (none? p1) (none? p2))
        (raise (ValueError [p1 p2 p-to])))
      (+ (dist-taxi p1 p2)
        (* 2 (bool (and (Creature.at p2) (!= p2 p-to))))))))
  (cut (second (searcher p-from p-to max-cost)) 1))

(defclass Bee [Monster] [
  name (NounPhrase "bumblebee")
  char "a"
  color-fg :yellow
  info-text "A jolly little insect that buzzes about aimlessly. Your standard-issue contestant protective gear will protect you from stings. Bees can still kind of get in the way, though."

  flying True

  act (fn [self]
    (or
      (.flee-from-player self)
      (wander self)
      (.wait self)))])

(defclass Cat [Monster] [
  name (NounPhrase "cat")
  char "f"
  color-fg :dark-orange
  info-text "A regal creature with little concern for you or your affairs. It moves when the mood strikes it, or to avoid dirtying its dainty paws."

  move-chance (/ 1 30)

  unpleasant? (fn [self pos]
    ; Check whether a tile has an unpleasant terrain or is in
    ; the range of a spooky tile.
    (or
      (. (Tile.at pos) unpleasant)
      (any (amap
        (. (Tile.at it) spooky)
        (disc-taxi pos G.spook-radius)))))

  act (fn [self] (block
    (when (.flee-from-player self)
      (ret))
    ; Usually just sit there. Occasionally, wander in a random
    ; direction. Avoid unpleasant tiles.
    (unless (and
        (or (.unpleasant? self self.pos)
          (chance self.move-chance))
        (or (wander self (λ (not (.unpleasant? self it))))
          (wander self)))
      (.wait self))))])

(defclass Dog [Monster] [
  name (NounPhrase "dog")
  char "d"
  color-fg :brown
  info-text "A clingy, fawning mongrel that will cheerfully chase you and get underfoot. Fortunately, it's not the sharpest cheese in the pantry."

  detect-player-range 12

  act (fn [self] (block
    (when (.flee-from-player self)
      (ret))
    ; If the player is close, try to chase after them, not very
    ; intelligently.
    (setv d (- G.player.pos self.pos))
    (when (<= (len-taxi d) self.detect-player-range)
      (when (= (len-cheb d) 1)
        ; We're adjacent.
        ; If we're orthogonally adjacent, just stay here.
        (when (= (len-taxi d) 1)
          (.wait self)
          (ret))
        ; Otherwise, we're diagonally adjacent. If possible, move
        ; to be orthogonally adjacent. (In taxicab geometry,
        ; orthogonal is closer than diagonal.)
        (for [part (shuffle [(Pos d.x 0) (Pos 0 d.y)])]
          (setv p-to (+ part self.pos))
          (when (room-for? Creature p-to)
            (.walk-to self p-to)
            (ret)))
        ; Otherwise, chill out.
        (.wait self)
        (ret))
      ; If we have a path to the player, use it.
      (setv path (find-path-thru-creatures self.pos G.player.pos
        self.detect-player-range))
      (when path
        (if (room-for? self (first path))
          (.walk-to self (first path))
          (.wait self))
        (ret)))
    ; Otherwise, wander.
    (or (wander self) (.wait self))))])

(defclass Snail [Monster] [
  name (NounPhrase "giant snail")
  char "S"
  color-fg :dark-green
  info-text "A mindless, oversized gastropod that slithers around the dungeon, leaving a trail of slime in its wake. It's very slow, but it isn't slowed any further by slime."

  walk-speed (meth [] (/ 1 4))
  slime-immune True

  act (meth []
    (when (instance? Floor (Tile.at @pos))
      (mset @pos (Slime)))
    (or
      (@flee-from-player)
      (wander @@)
      (@wait)))])

(defclass Spider [Monster] [
  name (NounPhrase "giant spider")
  char "s"
  color-fg :red
  info-text "It doesn't bite, but it leaves webs wherever it goes."

  web-immune True

  act (meth []
    (when (instance? Floor (Tile.at @pos))
      (mset @pos (Web)))
    (or
      (@flee-from-player)
      (wander @@)
      (@wait)))])

(defclass Golem [Monster] [
  name (NounPhrase "golem")
  char "g"
  color-fg :brown
  info-text "A massive clay humanoid given a semblance of life by magic. It's very slow and it moves in a fixed pattern, but it's too heavy to push past."

  walk-speed (meth [] (/ 1 5))
  change-dir-time (seconds 2)
  heavy True
  spook-immune True

  __init__ (meth [&optional pos]
    (.__init__ (super Golem @@) pos)
    (setv @dir None)
    None)

  act (meth []
    ; The usual call to @flee-from-player is omitted on purpose.
    ; Golems do not know fear.
    ;
    ; If we have no direction set, choose the orthogonal
    ; direction we can walk the farthest in.
    (unless @dir
      (setv @dir (first (maxes (shuffle Pos.ORTHS) (λ
        (setv n 1)
        (while (room-for? @@ (+ @pos (* n it)))
          (+= n 1))
        n)))))
    ; If we have room, walk forward. Otherwise, turn around.
    ; (When trapped, we'll just spend all our time turning back
    ; and forth.)
    (if (room-for? @@ (+ @pos @dir))
      (@walk-to (+ @pos @dir))
      (do
        (setv @dir (* -1 @dir))
        (@take-time @change-dir-time))))])

(defclass UmberHulk [Monster] [
  name (NounPhrase "umber hulk")
  char "U"
  color-fg :brown
  info-text "This creature is large and looks monstrous, like a giant bipedal insect, but what it hungers for is earth and stone. It wanders around gobbling up walls, hopefully creating some passages that are useful to you."

  dig-time (seconds 5)

  act (meth [] (block
    (when (@flee-from-player)
      (ret))
    (for [d (shuffle Pos.ORTHS)]
      (setv p (+ @pos d))
      (when (and (instance? Wall (mget p)) (none? (Creature.at p)))
        (mset p (Floor))
        (@take-time @dig-time)
        (@walk-to p)
        (ret)))
    (or (wander @@) (@wait))))])

(defclass Nymph [Monster] [
  name (NounPhrase "nymph")
  gender :female
  char "n"
  color-fg :dark-green
  info-text "A primal spirit of the forest disguised as a comely young woman. Nymphs are infatuated with man-made objects and have no compunctions about stealing, which makes sense when you consider that the typical nymph has lived in the woods for 800 years with no human contact until being dumped onto the set of a game show. Fortunately, they can only carry one thing at a time, and they tend to quickly lose interest in the objects they acquire."

  detect-item-range 8
  take-item-time (seconds 1)
  drop-item-time (seconds 1)

  __init__ (meth [&optional pos item]
    (.__init__ (super Nymph @@) pos)
    (setv @item None)
    (when item
      (@get-item item))
    (setv @interested-in-item-till (dict))
    None)

  get-item (meth [item]
    (setv @item item)
    (setv (get @interested-in-item-till item)
      (+ G.current-time (randexp-dl-div 3))))

  item-attractive? (meth [item]
    (or
      (not-in item @interested-in-item-till)
      (< G.current-time (get @interested-in-item-till item))))

  act (meth [] (block
    (when (@flee-from-player)
      (ret))
    (when (and @item (not (@item-attractive? @item)))
      ; We've gotten bored with this item. Drop it if we can.
      (whenn (drop-pos @pos)
        (@take-time @drop-item-time)
        (.move @item it)
        (when (seen @pos)
          (msg "{:The} drops {:a}." @@ @item))
        (setv @item None)
        (ret)))
    ; If we don't have an item, and there's an item we're not
    ; bored with (on the floor or in the player's inventory) in
    ; range that we can see, go to it.
    (when (not @item)
      (setv ps
        (sorted :key (λ (len (second it)))
        (shuffle
        (filt (second it)
        (amap (, it
          (find-path @pos it @detect-item-range))
        (filter (fn [p] (and
          (or
            (whenn (@gettable-item-at p) (and it (@item-attractive? it)))
            (and (= p G.player.pos) (not (@player-repulsive?))
              (afind-or (@item-attractive? it) G.inventory)))
          (in-los? @pos p)))
        (disc-taxi @pos @detect-item-range)))))))
      (when ps
        (setv [dest path] (first ps))
        (cond
          [(= dest @pos) (do
            ; Pick up the item here.
            (@take-time @take-item-time)
            (@get-item (Item.at @pos))
            (.move @item None)
            (when (seen @pos)
              (msg "{:The} picks up {:a}." @@ @item)))]
          [(and
              (= dest G.player.pos)
              (not (@player-repulsive?))
              (= (dist-cheb @pos dest) 1)
              (afind-or (@item-attractive? it) G.inventory)) (do
            ; We're adjacent to the player, and they have something
            ; we want. Steal it.
            (@take-time @take-item-time)
            (@get-item (choice (filt (@item-attractive? it) G.inventory)))
            (.remove G.inventory @item)
            (msg "{:The} stole {:your}." @@ @item))]
          [(room-for? @@ (first path))
            ; We have a usable path to an item. Take the next step.
            (@walk-to (first path))]
          [True
            (@wait)])
        (ret)))
    ; Otherwise, wander.
    (or (wander @@) (@wait))))])
