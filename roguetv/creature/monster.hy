(require kodhy.macros roguetv.macros)

(import
  pypaths.astar
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.map [Tile room-for? on-map]]
  [roguetv.creature [Creature]])

(defclass Monster [Creature] [
  ; A class for all non-player creatures.

  [walk-to (fn [self p-to]
    (unless (.walk-to (super Monster self) p-to)
      (raise (ValueError (.format "{} tried to walk where it couldn't: {}" self p-to)))))]])

(defn clear-neighbors [pos]
  (filt (room-for? Creature it)
    (amap (+ pos it) Pos.DIR8)))

(defn wander [cr &optional [okay? (λ True)]] (block
  ; Try to step in a random direction. Return a boolean indicating
  ; whether we succeeded.
  (setv neighbors (filt (okay? it) (clear-neighbors cr.pos)))
  (unless neighbors
    (ret False))
  (setv p-to (pick neighbors))
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

(defclass Cat [Monster] [
  [name (NounPhrase "cat")]
  [char "f"]
  [color-fg :dark-orange]
  [info-text "A regal creature with little concern for you or your affairs. It moves when the mood strikes it, or to avoid dirtying its dainty paws."]

  [move-chance (/ 1 30)]

  [act (fn [self] (block
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
      (setv path (find-path self.pos G.player.pos self.detect-player-range))
      (when path
        (.walk-to self (first path))
        (ret)))
    ; Otherwise, wander.
    (or (wander self) (.wait self))))]])
