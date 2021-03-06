(require [kodhy.macros [lc filt ecase]] [roguetv.macros [*]])

(import
  [math [log sqrt exp ceil]]
  random
  datetime
  [heidegger.pos [Pos]]
  [kodhy.util [T F signum seq keyword->str cat]]
  [roguetv.strings [hallucinated-announcer-names hallucinated-item-verbs]]
  [roguetv.globals :as G])

(defn real-timestamp []
  ; "Real" in the sense that this uses real time, not the game's
  ; simulated time.
  (.isoformat (datetime.datetime.utcnow)))

(defn logit [x]
  (log (/ x (- 1 x))))

(defn ilogit [x]
  (/ 1 (+ 1 (exp (- x)))))

(defn chance [x]
  (<= (random.random) x))

(defn randexp [median]
  (random.expovariate (/ (log 2) median)))

(defn randgeom [mean]
  (setv p (/ 1 (+ mean 1)))
  (int (ceil (- (/ (log (- 1 (random.random))) (log (- 1 p))) 1))))

(defn 1-in [n]
  (chance (/ 1 n)))

(defn shuffle [l]
  (setv l (list l))
  (random.shuffle l)
  l)

(defn randpop [l]
  (l.pop (random.randrange (len l))))

(defn values-sorted-by-keys [x]
  (list-comp v [[_ v] (sorted (.items x))]))

(defn seconds [x]
  ; Convert seconds to the internal time representation.
  (if x (max 1 (round (* G.clock-factor x))) 0))
    ; (max 1 …) ensures that no nonzero durations will be rounded
    ; to 0.
(defn minutes [x]
  (seconds (* 60 x)))
(defn round-to-second [duration]
  ; Rounds a duration (in the internal time representation)
  ; to the nearest second.
  (* (round (/ duration G.clock-factor))) G.clock-factor)

(defn minsec [x]
  (setv x (ceil (/ x G.clock-factor)))
  (.format "{}:{:02}" (// x 60) (% x 60)))

(defn show-duration [x &optional [trunc-to-sec F] [abbreviate F]]
  (setv parts [])
  (.append parts ["h" "hour" (// x (* 60 60 G.clock-factor))])
  (%= x (* 60 60 G.clock-factor))
  (.append parts ["min" "minute" (// x (* 60 G.clock-factor))])
  (%= x (* 60 G.clock-factor))
  (.append parts ["s" "second" (// x G.clock-factor)])
  (%= x G.clock-factor)
  (when trunc-to-sec
    (setv x 0))
  (.append parts [G.clock-unit-abbr G.clock-unit-name x])
  (.join " " (lc [[abbr name n] parts]
    n
    (.format "{} {}{}"
      n
      (if abbreviate abbr name)
      (if (and (not abbreviate) (!= n 1)) "s" "")))))

(defn show-round [number ndigits]
  (setv x (round number ndigits))
  (string (if (= x (int x)) (int x) x)))

(defn len-taxi [p]
  ; The length of a vector according to the taxicab norm (1-norm).
  ; So Pos.ORTHS have length 1 and Pos.DIAGS have length 2.
  ; In Rogue TV, taxicab geometry is the rule.
  (+ (abs p.x) (abs p.y)))
(defn dist-taxi [p1 p2]
  (len-taxi (- p1 p2)))

(defn len-euclid [p]
  ; The length of a vector according to the Euclidean norm (2-norm).
  ; So Pos.ORTHS have length 1 and Pos.DIAGS have length sqrt(2).
  (sqrt (+ (** p.x 2) (** p.y 2))))
(defn dist-euclid [p1 p2]
  (len-euclid (- p1 p2)))

(defn len-cheb [p]
  ; The length of a vector according to the Chebyshev norm (∞-norm).
  ; So Pos.ORTHS and Pos.DIAGS all have length 1.
  (max (abs p.x) (abs p.y)))
(defn dist-cheb [p1 p2]
  (len-cheb (- p1 p2)))

(defn adjacent? [p1 p2]
  (= (dist-cheb p1 p2) 1))

(defn line-bresen [p1 p2]
; Bresenham's line algorithm. Returns a list of Pos.
  (setv steep? (> (abs (- p2.y p1.y)) (abs (- p2.x p1.x))))
  (when steep?
    (setv p1 (Pos p1.y p1.x))
    (setv p2 (Pos p2.y p2.x)))
  (setv swapped F)
  (when (> p1.x p2.x)
    (setv [p1 p2] [p2 p1])
    (setv swapped T))
  (setv dx (- p2.x p1.x))
  (setv dy (- p2.y p1.y))
  (setv error (// dx 2))
  (setv y p1.y)
  (setv out [])
  (for [x (seq p1.x p2.x)]
    (.append out (if steep? (Pos y x) (Pos x y)))
    (-= error (abs dy))
    (when (< error 0)
      (+= y (signum dy))
      (+= error dx)))
  (when swapped
    (.reverse out))
  out)

(defn dl-time-limit [dl]
  (minutes (+ 3 (/ dl 2))))

(defn randexp-dl-div [divisor]
  (int (randexp (/ (dl-time-limit G.dungeon-level) divisor))))

(defn player? [cr]
  (is cr G.player))

(defn seen [pos]
  (get G.seen-map pos.x pos.y))

(defn msg [&rest args]
  (setv args (list args))
  (setv mtype (when (symbol? (first args))
    (.pop args 0)))
  (when mtype
    (setv announcer (ecase mtype
      ['tara
        (+ (if (hallu) G.hallucinated-tara "Tara") ":")]
      ['bob
        (+ (if (hallu) (get hallucinated-announcer-names G.hallucinated-tara) "Bob") ":")]
      ['aud
        "The audience"]))
    (setv (get args 0) (.format "{} {}"
      (color-xml announcer (get G.announcer-colors mtype))
      (get args 0))))
  (setv text (.format (first args) #* (rest args) :p G.player))
  (if (and G.message-log (= (get G.message-log -1 1) text))
    (+= (get G.message-log -1 0) 1)
    (.append G.message-log [1 text])))

(defn msgp [cr &rest args]
  (when (player? cr)
    (msg #* args)))

(defn you-dont-have-anything-to [verb]
  (msg "You don't have anything to {}." (if (hallu)
    (random.choice hallucinated-item-verbs)
    verb)))

(defn update-msg-highlighting []
  ; This saves the number of the last message and its repeat
  ; count. When more messages are printed, we'll highlight them
  ; if they're new messages or if the last message had its count
  ; increased.
  (setv G.message-log (cut G.message-log (- G.max-message-log-len)))
  (setv G.last-new-message-number (dec (len G.message-log)))
  (setv G.last-message-count (get G.message-log -1 0)))

(defn color-xml [text &optional fg bg]
; Formats a colored string for AttrStr. (It should already be
; escaped for XML.)
  (if (or fg bg)
    (.format "<c{}{}>{}</c>"
      (if fg (.format " fg='{}'" (keyword->str fg)) "")
      (if bg (.format " bg='{}'" (keyword->str bg)) "")
      text)
    text))

(setv information-G ((type "information-G" (, object) {
  "__getattr__" (fn [self name]
    (getattr G (.replace name "-" "_")))})))

(defn soil-fov []
  (setv G.fov-dirty? T))

(defn active-inv []
  (filt (.carry-effects-active? it) G.inventory))

(defn hallu []
  (.get-effect G.player (rtv-get creature.generic.Hallucinating)))
