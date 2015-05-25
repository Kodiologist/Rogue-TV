(require kodhy.macros roguetv.macros)

(import
  pypaths.astar
  [heidegger.pos [Pos]]
  [kodhy.util [ret]]
  [roguetv.english [TakesPronouns NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile room-for? on-map]])

(defclass Creature [Drawable MapObject TakesPronouns] [
  [extant []]
  [char "C"]

  [can-open-doors False]

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

  [wait (fn [self]
    (.take-time self 1))]

  [walk-to (fn [self p-to] (block
    (unless (and
        (on-map p-to)
        (.bump-into (Tile.at p-to) self)
        (not (. (Tile.at p-to) blocks-movement)))
      (ret False))
    (setv cr (Creature.at p-to))
    ; The player can push past other creatures, but other creatures
    ; can't push past the player or each other.
    (when cr
      (unless (player? self)
        (ret False))
      (.take-time self G.push-past-monster-time))
    ; Okay, we're clear to move.
    (setv p-from self.pos)
    (.step-out-of (Tile.at p-from) self p-to)
    (.take-time self (dist-taxi p-from p-to))
    (kwc .move self p-to :+clobber)
    (when cr
      (msg "You push past {:the}." cr)
      (.move cr p-from))
    (when (player? self)
      (rtv display.describe-tile self.pos))
    (.after-step-onto (Tile.at p-to) self p-from)
    True))]

  [act (fn [self]
    ; It's this creature's turn to act. Go wild, calling
    ; .take-time as needed.
    (msg "[No .act implemented for {}]" self)
    (.wait self))]])

(defclass Player [Creature] [
  [char "@"]
  [color-bg :yellow]

  [can-open-doors True]

  [move (fn [self p-to &optional [clobber False]]
    (.move (super Player self) p-to clobber)
    (soil-fov))]

  [act (fn [self]
    (rtv display.full-redraw)
    (setv old-clock-debt G.player.clock-debt-ms)
    (rtv actions.do-normal-command (rtv input.get-normal-command))
    (setv G.last-action-duration (/
      (- G.player.clock-debt-ms old-clock-debt)
      Creature.clock-factor)))]])

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
