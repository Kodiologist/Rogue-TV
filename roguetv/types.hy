(require [kodhy.macros [amap afind-or whenn ecase λ meth cmeth]])

(import
  [string [ascii-letters]]
  hashlib
  [random [choice]]
  xml.sax.saxutils
  [kodhy.util [T F mins merge-dicts]]
  [roguetv.strings [bob-too-bad hallucinated-object-strs]]
  [roguetv.english [NounPhraseNamed NounPhrase]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(defclass Drawable [object] [
  char None
  color-fg G.fg-color
  color-bg None

  ; These getters are trivial in Drawable, but may be overridden
  ; in subclasses.
  get-char (fn [self]
    self.char)
  get-color-fg (fn [self]
    self.color-fg)
  get-color-bg (fn [self]
    self.color-bg)

  xml-symbol (fn [self]
    (color-xml
      (xml.sax.saxutils.escape (.get-char self))
      (.get-color-fg self)
      (.get-color-bg self)))])

(defclass MapObject [object] [

  init-omap (classmethod (fn [self width height]
    (setv self.omap (amap (* [None] height) (range width)))))

  __init__ (fn [self &optional pos]
    ; 'pos' may be None whenever the object isn't currently
    ; on the map.
    (setv self.pos None)
    (.move self pos)
    None)

  move (fn [self p-to &optional [clobber F]]
    ; Set 'p-to' to None to remove the object from the map.
    ;
    ; If 'p-to' is not None, 'clobber' is true, and there's
    ; something already at 'p-to', remove it. Otherwise, moving
    ; onto a Pos where there's already something else is an
    ; error.
    (when self.pos
      (try
        (setv (get self.omap self.pos.x self.pos.y) None)
        (except [IndexError])))
          ; An IndexError may arise from the assignment if we've
          ; done a .move after an old position has become
          ; invalid. This is fine.
    (when p-to
      (whenn (get self.omap p-to.x p-to.y)
        (if clobber
          (it.move None)
          (raise (ValueError (.format
            "tried to move {} to {} where there was already {}"
            self p-to it)))))
      (setv (get self.omap p-to.x p-to.y) self))
    (setv self.pos p-to))

  at (classmethod (fn [self pos]
    (get self.omap pos.x pos.y)))])

(defclass Scheduled [object] [
  queue []
  queue-priority 0
    ; Should be an integer. Lower means acting sooner.

  schedule (meth []
    ; The object will first be able to act before any time
    ; passes, but after any previously existing objects (of the
    ; same or lesser .queue-priority) that are currently ready to
    ; act have acted.
    (setv @next-turn G.current-time)
    (.append @queue @@)
    (.sort @queue :key (λ it.queue-priority)))

  scheduled? (meth []
    (hasattr @@ "next_turn"))

  deschedule (meth []
    (when (@scheduled?)
      (.remove @queue @@)
      (del @next-turn)))

  take-time (meth [duration]
    (assert (integer? duration))
    (+= @next-turn duration))

  wait (meth []
    ; Convenience method for waiting 1 second.
    (@take-time (seconds 1)))

  act (meth []
    ; It's this object's turn to act. Go wild, calling
    ; .take-time as needed.
    (raise (ValueError (.format "No .act implemented for {}" (type @@)))))

  destroy (meth []
    (@deschedule))

  game-loop (classmethod (meth []
    (while T
      (setv actor (first (mins @queue (λ it.next-turn))))
        ; We use (first (mins …)) instead of just (min …) because
        ; the behavior of Python's `min` is undefined for ties.
      (assert (>= actor.next-turn G.current-time))
      (setv G.current-time actor.next-turn)
      (.act actor)
      (when G.endgame
        (break)))))])

(defclass LevelTimer [Scheduled] [
  queue-priority -3

  act (meth []
    (setv seconds-left (// (- G.time-limit G.current-time) G.clock-factor))
    (cond
      [seconds-left
        (msg 'aud "chants \"{}!\""
          (get
            (if (hallu)
              ["Fortune" "Of" "Wheel" "Pants" "Fish"]
              ["One" "Two" "Three" "Four" "Five"])
            (dec seconds-left)))]
      [(and
        ; Hallucination has a chance of preventing a game-over, by
        ; extending the time limit just as it runs out.
        ; This works only once per game.
          (hallu)
          (not G.hallu-prevented-gameover)
          (1-in G.hallu-prevent-gameover-1in))
        (msg 'bob "But what if xXx_{p:}_xXx is not kill?")
        (set-time-limit (+ G.time-limit (seconds G.hallu-prevent-gameover-extra-seconds)))
        (setv G.hallu-prevented-gameover T)]
      [T
        (msg 'tara "Alas! {p:The} is out of time. {p:He} may keep only half {p:his} winnings.")
        (msg 'bob (choice bob-too-bad))
        (setv G.time-limit None)
        (setv G.endgame :out-of-time)])
    (@wait))])

(defn set-time-limit [x]
  (setv G.time-limit x)
  (unless (afind-or (instance? LevelTimer it) Scheduled.queue)
    (.insert Scheduled.queue 0 (LevelTimer)))
  (assert (instance? LevelTimer (first Scheduled.queue)))
  (setv (. (first Scheduled.queue) next-turn)
    (- G.time-limit G.super-low-time-threshold)))

(defclass Generated [object] [
  level-lo 0
  level-hi None
  rarity :common
  unique F

  __init__ (meth []
    (when @unique
      (setv tname (. (type @@) __name__))
      (when (in tname G.uniques-generated)
        (raise (ValueError (+ "Tried to generate a second instance of a unique type: " tname))))
      (.append G.uniques-generated tname))
    None)

  generation-weight (cmeth [dl &optional [in-chest F]]
    (when in-chest
      ; Chests generate deeper items.
      (+= dl 3))
    (if (or (= @rarity :nongen) (and @unique (in @__name__ G.uniques-generated)))
      0
      (*
        (/ 1 (ecase @rarity
          ; Chests make uncommon and rare items more common.
          [:common   1]
          [:uncommon (if in-chest 2  4)]
          [:rare     (if in-chest 8 16)]))
        (/ 1 (cond
          [(< dl @level-lo)
            (inc (- @level-lo dl))]
          [(and (not (none? @level-hi)) (> dl @level-hi))
            (inc (- dl @level-hi))]
          [T 1])))))])

(defclass Hallucination [NounPhraseNamed Drawable] [
  all {"item" {} "monster" {}}

  __init__ (meth [kind halluid]
    (assert (in kind ["item" "monster"]))

    (setv [gender stem] (cond
      [(.startswith halluid "m:") [:male   (cut halluid (len "m:"))]]
      [(.startswith halluid "f:") [:female (cut halluid (len "f:"))]]
      [True                       [:neuter halluid]]))

    (defn match [s &kwargs kwargs]
      (when (.startswith stem s)
        (apply NounPhrase
          [(cut stem (len s))]
          (merge-dicts {"gender" gender} kwargs))))
    (setv @name (or
      (when (= stem "the thing that your aunt gave you")
        (NounPhrase "thing that your aunt gave you"
          :plural "things that your aunt gave you"
          :the-proper T))
      (match "a "
        :article "a")
      (match "an "
        :article "an")
      (match "some "
        :mass T :unit "thingies")
      (match "the "
        :the-proper T)
      (NounPhrase stem :bare-proper T :gender gender)))

    (setv legal-chars (ecase kind
      ["item" "/[]()*!$"]
      ["monster" ascii-letters]))
    (setv @char (get legal-chars (%
      ; Randomly choose a character for the hallucinated object,
      ; using `halluid` as the seed (so it's consistent between
      ; launches of Rogue TV).
      (int.from-bytes :byteorder "big"
        (.digest (hashlib.md5 (.encode halluid "UTF-8"))))
      (len legal-chars))))
    (setv @info (get hallucinated-object-strs kind halluid))
    (assert (not-in halluid (get Hallucination.all kind)))
    (setv (get Hallucination.all kind halluid) @@))])

(for [[kind d] (.items hallucinated-object-strs) halluid d]
  (Hallucination kind halluid))

(defclass CanBeHallucinated [] [
  hallu-kind None

  __init__ (meth []
    (setv @halluid None))

  hallucinate (meth []
    (unless @halluid
      (setv @halluid (random.choice
        (list (.keys (get Hallucination.all @hallu-kind))))))
    (get Hallucination.all @hallu-kind @halluid))

  get-name (meth []
    (if (hallu)
      (. (@hallucinate) name)
      (.get-name (super CanBeHallucinated @@))))

  get-char (meth []
    (if (hallu)
      (. (@hallucinate) char)
      (.get-char (super CanBeHallucinated @@))))

  get-color-fg (meth []
    (if (hallu)
      G.hallucinated-object-color
      (.get-color-fg (super CanBeHallucinated @@))))])
