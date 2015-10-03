(require kodhy.macros)

(import
  xml.sax.saxutils
  [kodhy.util [retf]]
  [roguetv.globals :as G]
  [roguetv.util [*]])

(defclass Drawable [object] [
  [char None]
  [color-fg G.fg-color]
  [color-bg None]

  [xml-symbol (fn [self]
    (color-xml
      (xml.sax.saxutils.escape self.char)
      self.color-fg
      self.color-bg))]])

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
  clock-factor 1000
    ; This should not be overridden by subclasses.

  schedule (meth []
    (setv @clock-debt-ms 0)
    (.append @queue @))

  scheduled? (meth []
    (hasattr @ "clock_debt_ms"))

  deschedule (meth []
    (when (@scheduled?)
      (.remove @queue @)
      (del @clock-debt-ms)))

  take-time (meth [duration]
    ; Mark the object as accumulating 'duration' seconds of clock debt.
    (when duration
      (+= @clock-debt-ms (round (* @clock-factor duration)))))

  wait (meth []
    (@take-time 1))

  act (meth []
    ; It's this object's turn to act. Go wild, calling
    ; .take-time as needed.
    (raise (ValueError (.format "No .act implemented for {}" (type @)))))

  destroy (meth []
    (@deschedule))

  run-schedule (classmethod (meth []
    ; Give everything in the queue a chance to act, increment
    ; the game time by 1 second, and remove 1 second of clock debt.
    (while True
      (setv something-acted False)
      (for [x (list @queue)]
        (while (and (.scheduled? x) (< x.clock-debt-ms @clock-factor))
            ; We have to constantly check that this object is
            ; scheduled in case it disappeared (particularly, if
            ; the player went to a new level) since we started
            ; the whole loop.
          (.act x)
          (setv something-acted True)
          (when G.endgame
            (retf :game-loop))))
      ; Re-loop through the scheduling queue if anything acted,
      ; in case some (.act x) has added a new object to the
      ; queue.
      (unless something-acted
        (break)))
    (+= G.current-time 1)
    (for [x @queue]
      (-= x.clock-debt-ms @clock-factor)
      (assert (>= x.clock-debt-ms 0))))))

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
