(require kodhy.macros)

(import
  [random [choice]]
  xml.sax.saxutils
  [kodhy.util [mins]]
  [roguetv.strings [bob-too-bad]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(defclass Drawable [object] [
  [char None]
  [color-fg G.fg-color]
  [color-bg None]

  ; These getters are trivial in Drawable, but may be overridden
  ; in subclasses.
  [get-char (fn [self]
    self.char)]
  [get-color-fg (fn [self]
    self.color-fg)]
  [get-color-bg (fn [self]
    self.color-bg)]

  [xml-symbol (fn [self]
    (color-xml
      (xml.sax.saxutils.escape (.get-char self))
      (.get-color-fg self)
      (.get-color-bg self)))]])

(defclass MapObject [object] [

  [init-omap (classmethod (fn [self width height]
    (setv self.omap (amap (* [None] height) (range width)))))]

  [__init__ (fn [self &optional pos]
    ; 'pos' may be None whenever the object isn't currently
    ; on the map.
    (setv self.pos None)
    (.move self pos)
    None)]

  [move (fn [self p-to &optional [clobber False]]
    ; Set 'p-to' to None to remove the object from the map.
    ;
    ; If 'p-to' is not None, 'clobber' is true, and there's
    ; something already at 'p-to', remove it. Otherwise, moving
    ; onto a Pos where there's already something else is an
    ; error.
    (when self.pos
      (try
        (setv (get self.omap self.pos.x self.pos.y) None)
        (catch [_ IndexError])))
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
    (setv self.pos p-to))]

  [at (classmethod (fn [self pos]
    (get self.omap pos.x pos.y)))]])

(defcls Scheduled [object]
  queue []

  schedule (meth []
    ; The object will first be able to act before any time passes,
    ; but after any previously existing objects that are currently
    ; ready to act have acted.
    (setv @next-turn G.current-time)
    (.append @queue @))

  scheduled? (meth []
    (hasattr @ "next_turn"))

  deschedule (meth []
    (when (@scheduled?)
      (.remove @queue @)
      (del @next-turn)))

  take-time (meth [duration]
    (+= @next-turn (seconds duration)))

  wait (meth []
    ; Convenience method for waiting 1 second.
    (@take-time 1))

  act (meth []
    ; It's this object's turn to act. Go wild, calling
    ; .take-time as needed.
    (raise (ValueError (.format "No .act implemented for {}" (type @)))))

  destroy (meth []
    (@deschedule))

  game-loop (classmethod (meth []
    (while True
      (setv actor (first (mins @queue (λ it.next-turn))))
        ; We use (first (mins …)) instead of just (min …) because
        ; the behavior of Python's `min` is undefined for ties.
      (.remove @queue actor)
      (.append @queue actor)
      (assert (>= actor.next-turn G.current-time))
      (setv G.current-time actor.next-turn)
      (.act actor)
      (when G.endgame
        (break))))))

(defcls LevelTimer [Scheduled]
  act (meth []
    (setv seconds-left (// (- G.time-limit G.current-time) G.clock-factor))
    (if seconds-left
      (msg :aud "chants \"{}!\""
        (get [None "One" "Two" "Three" "Four" "Five"] seconds-left))
      (do
        (msg :tara "Alas! {p:The} is out of time. {p:He} may keep only half {p:his} winnings.")
        (msg :bob (choice bob-too-bad))
        (setv G.time-limit None)
        (setv G.endgame :out-of-time)))
    (@wait)))

(defn set-time-limit [x]
  (setv G.time-limit x)
  (unless (afind-or (instance? LevelTimer it) Scheduled.queue)
    (.insert Scheduled.queue 0 (LevelTimer)))
  (assert (instance? LevelTimer (first Scheduled.queue)))
  (setv (. (first Scheduled.queue) next-turn)
    (- G.time-limit G.super-low-time-threshold)))

(defcls Generated [object]
  level-lo 0
  level-hi G.max-dungeon-level
  rarity :common
  unique False

  __init__ (meth []
    (when @unique
      (setv tname (. (type @) __name__))
      (when (in tname G.uniques-generated)
        (raise (ValueError (+ "Tried to generate a second instance of a unique type: " tname))))
      (.append G.uniques-generated tname))
    None)

  generation-weight (cmeth [dl]
    (if (or (= @rarity :nongen) (and @unique (in @__name__ G.uniques-generated)))
      0
      (*
        (/ 1 (ecase @rarity
          [:common    1]
          [:uncommon  4]
          [:rare     16]))
        (/ 1 (cond
          [(< dl @level-lo) (inc (- @level-lo dl))]
          [(> dl @level-hi) (inc (- dl @level-hi))]
          [True 1]))))))
