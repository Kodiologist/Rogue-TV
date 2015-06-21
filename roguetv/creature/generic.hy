(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [TakesPronouns]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject]]
  [roguetv.map [Tile on-map]])

(defclass Creature [Drawable MapObject TakesPronouns] [
  [extant []]
  [char "C"]
  [info-text "[Missing info text]"]

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

  [information (fn [self]
    (setv s (.format "\n  {:a}\n\n{}"
      self
      self.info-text))
    (apply .format [s] (. (type self) __dict__)))]

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
