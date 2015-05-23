(require kodhy.macros roguetv.macros)

(import
  pypaths.astar
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [TakesPronouns NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [room-for?]])

(defclass Creature [Drawable MapObject TakesPronouns] [
  [extant []]
  [char "C"]

  [__init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (setv self.clock-debt-ms 0)
    (self.reset-ice-slipping)
    (.append Creature.extant self)
    None)]

  [reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))]

  [clock-factor 1000]
  [take-time (fn [self duration]
    ; Mark the creature as accumulating 'duration' seconds of
    ; clock debt.
    (when duration
      (+= self.clock-debt-ms (round (* self.clock-factor duration)))
      (when self.ice-slip-time
        ; The creature takes some extra time slipping.
        (setv slip-time self.ice-slip-time)
        (msgp self "You take a moment to steady yourself on the ice.")
        (self.reset-ice-slipping)
        (.take-time self slip-time))))]

  [act (fn [self]
    ; It's this creature's turn to act. Go wild, calling
    ; .take-time as needed.
    (msg "[No .act implemented for {}]" self)
    (.take-time self 1))]])

(defclass Player [Creature] [
  [char "@"]
  [color-bg :yellow]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]])

(defn clear-neighbors [pos]
  (filt (room-for? Creature it)
    (amap (+ pos it) Pos.DIR8)))

(defn wander [cr] (block
  ; Try to step in a random direction. Return a boolean indicating
  ; whether we succeeded.
  (setv neighbors (clear-neighbors cr.pos))
  (unless neighbors
    (ret False))
  (setv p-to (pick neighbors))
  (.take-time cr (len-taxi (- p-to cr.pos)))
  (.move cr p-to)
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

(defclass Cat [Creature] [
  [name (NounPhrase "cat")]
  [char "f"]
  [color-fg :dark-orange]

  [move-chance (/ 1 30)]

  [act (fn [self] (block
    ; Usually just sit there. Occasionally, wander in a random
    ; direction.
    (unless (and (chance self.move-chance) (wander self))
      (.take-time self 1))))]])

(defclass Dog [Creature] [
  [name (NounPhrase "dog")]
  [char "d"]
  [color-fg :brown]

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
          (.take-time self 1)
          (ret))
        ; Otherwise, we're diagonally adjacent. If possible, move
        ; to be orthogonally adjacent. (In taxicab geometry,
        ; orthogonal is closer than diagonal.)
        (for [part (shuffle [(Pos d.x 0) (Pos 0 d.y)])]
          (setv p-to (+ part self.pos))
          (when (room-for? Creature p-to)
            (.take-time self (dist-taxi self.pos p-to))
            (.move self p-to)
            (ret)))
        ; Otherwise, chill out.
        (.take-time self 1)
        (ret))
      ; If we have a path to the player, use it.
      (setv path (find-path self.pos G.player.pos self.detect-player-range))
      (when (len path)
        (.take-time self (dist-taxi self.pos (first path)))
        (.move self (first path))
        (ret)))
    ; Otherwise, wander.
    (or (wander self) (.take-time self 1))))]])
