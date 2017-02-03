(require [kodhy.macros [afind-or whenn block meth cmeth]] [roguetv.macros [*]])

(import
  random
  [kodhy.util [T F ret]]
  [roguetv.english [NounPhraseNamed]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.types [Drawable MapObject Scheduled]]
  [roguetv.map [Tile on-map mget room-for? outer-corner-pos circ-taxi disc-taxi]]
  [roguetv.item [Item]])

(defclass Creature [Drawable MapObject Scheduled NounPhraseNamed] [
  escape-xml-in-np-format T
  char "C"
  info-text "[Missing info text]"

  can-open-doors F
  flying F
  slime-immune F
  web-immune F
  spook-immune F
  heavy F
    ; A heavy creature can't be pushed past by the player.

  __init__ (fn [self &optional pos]
    (MapObject.__init__ self pos)
    (Scheduled.__init__ self)
    (self.schedule)
    (self.reset-ice-slipping)
    None)

  get-info-text (fn [self]
    self.info-text)

  reset-ice-slipping (fn [self]
    (setv self.ice-slip-time 0)
    (setv self.ice-slip-towards None))

  ice-immune (fn [self]
    F)

  can-see-contents (fn [self container-tile]
    (not container-tile.opaque-container))

  visible-item-at (fn [self p]
    ; Returns 0 for being able to see there's no item, and None
    ; if the creature can't see whether there is one or not.
    ; N.B. This does *not* check whether the creature can see the
    ; tile in the first place.
    (setv t (Tile.at p))
    (when (or (not t.container) (.can-see-contents self t))
      (or (Item.at p) 0)))

  gettable-item-at (fn [self p]
    (and (not (. (Tile.at p) container)) (Item.at p)))

  information (fn [self]
    (.format "\n  {} {:a}\n\n{}"
      (.xml-symbol self)
      self
      (apply .format [(.get-info-text self)] (. (type self) __dict__))))

  take-time (fn [self duration]
    (.take-time Scheduled self duration)
    (when (and duration self.ice-slip-time)
      ; The creature takes some extra time slipping.
      (setv slip-time self.ice-slip-time)
      (msgp self "You take a moment to steady yourself on the ice.")
      (self.reset-ice-slipping)
      (.take-time self slip-time)))

  move (fn [self p-to &optional [clobber F]]
    (setv p-from self.pos)
    (MapObject.move self p-to clobber)
    (unless (none? self.pos)
      (.after-entering (Tile.at self.pos) self p-from)))

  walk-to (fn [self p-to] (block
    (setv p-from self.pos)
    (setv dist (.walk-dist self p-from p-to))
    (unless (.bump-into (mget p-to) self)
      (ret F))
    (when (player? self)
      (whenn (afind-or it.superheavy (active-inv))
        (msg "You can't move an inch so long as you're clinging to {:the}." it)
        (ret F))
      (when (not (on-map p-to))
        ; If the player has an item that allows them to wrap
        ; around the map, apply it.
        (when (and (in p-to (outer-corner-pos))
            (afind-or (or it.carry-mapwrap-eastwest it.carry-mapwrap-northsouth) (active-inv)))
          (msg 'tara "Please don't try to wrap through a corner of the dungeon, {p}. That could tear the spacetime continuum and destroy the universe.")
          (ret F))
        (for [[pa item-attr max-coord d-neg d-pos] [
            ["x" "carry_mapwrap_eastwest" G.map-width Pos.WEST Pos.EAST]
            ["y" "carry_mapwrap_northsouth" G.map-height Pos.SOUTH Pos.NORTH]]]
          (unless (and
              (in (getattr p-to pa) [-1 max-coord])
              (afind-or (getattr it item-attr) (active-inv)))
            (continue))
          (setattr p-to pa
            (if (= (getattr p-to pa) -1) (dec max-coord) 0))
          (setv d (if (getattr p-to pa) d-neg d-pos))
          (while T
            (when (room-for? self p-to)
              (break))
            (+= p-to d)
            (when (= (getattr p-to pa) (getattr p-from pa))
              (.take-time self (seconds (/ dist (self.walk-speed None))))
              (msg 'tara "Looks like {p} doesn't have room to wrap around the level here.")
              (ret F))))))
    (when (or
        (not (on-map p-to))
        (and (. (Tile.at p-to) blocks-movement)
          (not (and (player? self) (or
            (.get-effect self Passwall)
            G.always-passwall)))))
      (when (and (player? self) (.get-effect self Confusion))
        (msg "You bump into {:the}." (mget p-to))
        (.take-time self self.confusion-bump-time))
      (ret F))
    (setv cr (Creature.at p-to))
    ; The player can push past other creatures, but other creatures
    ; can't push past the player or each other.
    (when cr
      (unless (player? self)
        (ret F))
      (when cr.heavy
        (msg "You're far too puny to push past {:the}." cr)
        (ret F))
      (.take-time self self.push-past-monster-time))
    ; Okay, we're clear to move.
    (.step-out-of (Tile.at p-from) self p-to)
    (.take-time self (seconds (/ dist (self.walk-speed p-to))))
      ; Hence, a creature with walk-speed 1 takes 1 second to walk
      ; 1 unit of distance.
    (when cr
      (push-past-msg cr))
    (.move self p-to :clobber T)
    (when cr
      (.move cr p-from))
    (for [p (disc-taxi p-to G.spook-radius)]
      (when (and (. (mget p) spooky) (not self.spook-immune))
        (.take-time self (.spook-time self))
        (when (and (player? self) (not (. (mget p) player-noticed-spook)))
          (setv (. (mget p) player-noticed-spook) T)
          (msg "The hair on the back of your neck stands up."))))
    (when (player? self)
      (rtv display.describe-tile self.pos))
    (.after-step-onto (Tile.at p-to) self p-from)
    T))

  get-effect (fn [self effect-cls]
    None)

  walk-dist (fn [self p-from p-to]
    (dist-taxi p-from p-to))

  walk-speed (fn [self p-to]
    ; Return the applicable multiplier for the creature's walking
    ; speed.
    1)

  spook-time (fn [self]
    (seconds (random.randint 1 (inc G.dungeon-level))))])

(defn push-past-msg [cr]
  (setv verb (if (hallu)
    (random.choice [
      "rek" "no-scope" "quickscope" "rek" "MLG" "pwn" "blaze"
      "friendzone" "faze" "meme" "accidentally"
      "You set up {:the} the bomb."])
    "push past"))
  (msg 
    (if (in "{" verb)
      verb
      (+ "You " verb " {:the}."))
    cr))

(defclass Effect [Scheduled] [
; Despite that this class is in creature.generic instead of
; creature.player, only the player can have effects.

  queue-priority -2

  status "???"
    ; Text shown in the status bar.
  hallu-status "???"
    ; Text show in the status bar during hallucination.
  end-msg None

  __init__ (meth [duration]
    (@schedule)
    (@take-time duration)
    None)

  add-to-player (cmeth [duration start-msg lengthen-msg]
    (setv e (.get-effect G.player @@))
    ; If the player already has an effect of this kind,
    ; the new duration is added to the old one.
    (if e
      (do
        (lengthen-msg)
        (.take-time e duration))
      (do
        (start-msg)
        (.append G.player.effects (@@ duration)))))

  act (meth []
    (@destroy))

  destroy (meth []
    (msg @end-msg)
    (.remove G.player.effects @@)
    (.destroy (super Effect @@)))])

(defclass Stink [Effect] [
  status "PU"
  hallu-status "Ugly"
  end-msg "You smell presentable again."])

(defclass Haste [Effect] [
  status "Fast"
  hallu-status "Sanic"
  end-msg "The rush of energy fades."])

(defclass Confusion [Effect] [
  status "Conf"
  hallu-status "Tired"
  end-msg "Your mind clears."])

(defclass Strength [Effect] [
  status "Str"
  hallu-status "FaZe"
  end-msg "You feel like a 98-pound weakling."])
    ; Charles Atlas ads

(defclass Passwall [Effect] [
  status "Pass"
  hallu-status "Spooky"
  end-msg "You feel solid again."

  destroy (meth []
    (.destroy (super Passwall @@))
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
        (msg 'tara "Oh no! Is {p:the} trapped inside {:the}?" (Tile.at G.player.pos)))))])

(defclass Hallucinating [Effect] [
  status "Hallu"
  hallu-status "MLG"
  end-msg "Everything looks SO boring now."])
