(require kodhy.macros roguetv.macros)

(import
  [random [choice]]
  pypaths.astar
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile Floor Slime room-for? mset on-map]]
  [roguetv.creature [Creature Stink]])

(defclass Monster [Creature] [
  ; A class for all non-player creatures.

  [walk-to (fn [self p-to]
    (unless (.walk-to (super Monster self) p-to)
      (raise (ValueError (.format "{} tried to walk where it couldn't: {}" self p-to)))))]

  [flee-from-stink (fn [self] (block
    ; If the player stinks and we're in range of the stench, try
    ; to run away (not very intelligently), and return True.
    ; Otherwise, return False.
    (unless (and
        (.has-effect G.player Stink)
        (<= (dist-taxi self.pos G.player.pos) G.stink-range))
      (ret False))
    (setv neighbors (kwc sorted
      (shuffle (clear-neighbors self.pos))
      :key (λ (,
        (- (/ (dist-taxi it G.player.pos) (dist-taxi it self.pos)))
        (dist-taxi it self.pos)))))
    (if (and neighbors (>
        (dist-taxi (first neighbors) G.player.pos)
        (dist-taxi self.pos G.player.pos)))
      (.walk-to self (first neighbors))
      (.wait self))
    True))]])

(defn clear-neighbors [pos]
  (filt (room-for? Creature it)
    (amap (+ pos it) Pos.DIR8)))

(defn wander [cr &optional [okay? (λ True)]] (block
  ; Try to step in a random direction. Return a boolean indicating
  ; whether we succeeded.
  (setv neighbors (filt (okay? it) (clear-neighbors cr.pos)))
  (unless neighbors
    (ret False))
  (setv p-to (choice neighbors))
  (.walk-to cr p-to)
  True))

(defn find-path [p-from p-to &optional [max-cost (int 1e6)]]
  (setv searcher (kwc pypaths.astar.pathfinder
    :neighbors (fn [p] (+ (clear-neighbors p)
      (if (adjacent? p p-to) [p-to] [])))
    :distance dist-euclid
      ; This is the heuristic. Because the 2-norm ≤ the 1-norm,
      ; Euclidean distance is an admissible heuristic for taxicab
      ; geometry.
    :cost dist-taxi))
  (slice (second (searcher p-from p-to max-cost)) 1))

(defn find-path-thru-creatures [p-from p-to &optional [max-cost (int 1e6)]]
  (setv searcher (kwc pypaths.astar.pathfinder
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
  (slice (second (searcher p-from p-to max-cost)) 1))

(defclass Bee [Monster] [
  [name (NounPhrase "bumblebee")]
  [char "a"]
  [color-fg :yellow]
  [info-text "A jolly little insect that buzzes about aimlessly. Your standard-issue contestant protective gear will protect you from stings. Bees can still kind of get in the way, though."]

  [flying True]

  [act (fn [self]
    (or
      (.flee-from-stink self)
      (wander self)
      (.wait self)))]])

(defclass Cat [Monster] [
  [name (NounPhrase "cat")]
  [char "f"]
  [color-fg :dark-orange]
  [info-text "A regal creature with little concern for you or your affairs. It moves when the mood strikes it, or to avoid dirtying its dainty paws."]

  [move-chance (/ 1 30)]

  [act (fn [self] (block
    (when (.flee-from-stink self)
      (ret))
    ; Usually just sit there. Occasionally, wander in a random
    ; direction. Avoid unpleasant tiles.
    (unless (and
        (or (. (Tile.at self.pos) unpleasant)
          (chance self.move-chance))
        (or (wander self (λ (not (. (Tile.at it) unpleasant))))
          (wander self)))
      (.wait self))))]])

(defclass Dog [Monster] [
  [name (NounPhrase "dog")]
  [char "d"]
  [color-fg :brown]
  [info-text "A clingy, fawning mongrel that will cheerfully chase you and get underfoot. Fortunately, it's not the sharpest cheese in the pantry."]

  [detect-player-range 12]

  [act (fn [self] (block
    (when (.flee-from-stink self)
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
    (or (wander self) (.wait self))))]])

(defcls Snail [Monster]
  name (NounPhrase "giant snail")
  char "S"
  color-fg :dark-green
  info-text "An oversized mindless gastropod that slithers around the dungeon, leaving a trail of slime in its wake. It is very slow, but it is not slowed any further by slime."

  walk-speed (meth [] (/ 1 4))
  slime-immune True

  act (meth []
    (when (instance? Floor (Tile.at @pos))
      (mset @pos (Slime 5)))
    (or
      (@flee-from-stink)
      (wander @)
      (@wait))))
