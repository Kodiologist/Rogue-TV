(require kodhy.macros roguetv.macros)

(import
  [kodhy.util [ret]]
  [roguetv.english [NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject Scheduled]]
  [roguetv.map [Tile on-map mget room-for? circ-taxi]]
  [roguetv.item [Item]])

(defclass Creature [Drawable MapObject Scheduled NounPhraseNamed] [
  [escape-xml-in-np-format True]
  [char "C"]
  [info-text "[Missing info text]"]

  [can-open-doors False]
  [flying False]
  [slime-immune False]
  [web-immune False]

  [__init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (Scheduled.__init__ self)
    (self.schedule)
    (self.reset-ice-slipping)
    None)]

  [reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))]

  [ice-immune (fn [self]
    False)]

  [can-see-contents (fn [self container-tile]
    (not container-tile.opaque-container))]

  [visible-item-at (fn [self p]
    ; Returns 0 for being able to see there's no item, and None
    ; if the creature can't see whether there is one or not.
    ; N.B. This does *not* check whether the creature can see the
    ; tile in the first place.
    (setv t (Tile.at p))
    (when (or (not t.container) (.can-see-contents self t))
      (or (Item.at p) 0)))]

  [gettable-item-at (fn [self p]
    (and (not (. (Tile.at p) container)) (Item.at p)))]

  [information (fn [self]
    (.format "\n  {} {:a}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format [self.info-text] (. (type self) __dict__))))]

  [take-time (fn [self duration]
    (.take-time Scheduled self duration)
    (when (and duration self.ice-slip-time)
      ; The creature takes some extra time slipping.
      (setv slip-time self.ice-slip-time)
      (msgp self "You take a moment to steady yourself on the ice.")
      (self.reset-ice-slipping)
      (.take-time self slip-time)))]

  [move (fn [self p-to &optional [clobber False]]
    (setv p-from self.pos)
    (MapObject.move self p-to clobber)
    (unless (none? self.pos)
      (.after-entering (Tile.at self.pos) self p-from)))]

  [walk-to (fn [self p-to] (block
    (unless (.bump-into (mget p-to) self)
      (ret False))
    (when (player? self) (whenn (afind-or it.superheavy (active-inv))
      (msg "You can't move an inch so long as you're clinging to {:the}." it)
      (ret False)))
    (when (or
        (not (on-map p-to))
        (and (. (Tile.at p-to) blocks-movement)
          (not (and (player? self) (or
            (.get-effect self Passwall)
            G.always-passwall)))))
      (when (and (player? self) (.get-effect self Confusion))
        (msg "You bump into {:the}." (mget p-to))
        (.take-time self self.confusion-bump-time))
      (ret False))
    (setv cr (Creature.at p-to))
    ; The player can push past other creatures, but other creatures
    ; can't push past the player or each other.
    (when cr
      (unless (player? self)
        (ret False))
      (.take-time self self.push-past-monster-time))
    ; Okay, we're clear to move.
    (setv p-from self.pos)
    (.step-out-of (Tile.at p-from) self p-to)
    (.take-time self (seconds (/
      (.walk-dist self p-from p-to)
      (self.walk-speed))))
      ; Hence, a creature with walk-speed 1 takes 1 second to walk
      ; 1 unit of distance.
    (when cr
      (msg "You push past {:the}." cr))
    (kwc .move self p-to :+clobber)
    (when cr
      (.move cr p-from))
    (when (player? self)
      (rtv display.describe-tile self.pos))
    (.after-step-onto (Tile.at p-to) self p-from)
    True))]

  [get-effect (fn [self effect-cls]
    None)]

  [walk-dist (fn [self p-from p-to]
    (dist-taxi p-from p-to))]

  [walk-speed (fn [self]
    ; Return the applicable multiplier for the creature's walking
    ; speed.
    1)]])

(defcls Effect [Scheduled]
; Despite that this class is in creature.generic instead of
; creature.player, only the player can have effects.

  queue-priority -2

  status "???"
    ; Text shown in the status bar.
  end-msg None

  __init__ (meth [duration]
    (@schedule)
    (@take-time duration)
    None)

  add-to-player (classmethod (meth [duration start-msg lengthen-msg]
    (setv e (.get-effect G.player @))
    ; If the player already has an effect of this kind,
    ; the new duration is added to the old one.
    (if e
      (do
        (lengthen-msg)
        (.take-time e duration))
      (do
        (start-msg)
        (.append G.player.effects (@ duration))))))

  act (meth []
    (@destroy))

  destroy (meth []
    (msg @end-msg)
    (.remove G.player.effects @)
    (.destroy (super Effect @))))

(defcls Stink [Effect]
  status "PU"
  end-msg "You smell presentable again.")

(defcls Haste [Effect]
  status "Fast"
  end-msg "The rush of energy fades.")

(defcls Confusion [Effect]
  status "Conf"
  end-msg "Your mind clears.")

(defcls Strength [Effect]
  status "Str"
  end-msg "You feel like a 98-pound weakling.")
    ; Charles Atlas ads

(defcls Passwall [Effect]
  status "Pass"
  end-msg "You feel solid again."

  destroy (meth []
    (.destroy (super Passwall @))
    (when (. (Tile.at G.player.pos) blocks-movement)
      (block
        ; Passwall just ended while the player was in a solid
        ; obstacle. Eject them to the nearest clear tile.
        (for [r (seq 1 (+ G.map-width G.map-height))]
          (for [p (shuffle (circ-taxi G.player.pos r))]
            (when (room-for? Creature p)
              (msg "As you materialize, you are ejected from {:the}." (Tile.at G.player.pos))
              (.move G.player p)
              (ret))))
        ; There's no room anywhere on the level!
        (msg :tara "Oh no! Is {p:the} trapped inside {:the}?" (Tile.at G.player.pos))))))
