(require kodhy.macros)

(import
  [math [sqrt exp]]
  random
  [heidegger.pos [Pos]]
  [kodhy.util [signum seq keyword->str]]
  [roguetv.globals :as G])

(defn chance [x]
  (<= (random.random) x))

(defn randpois [mean]
; Poisson-distributed random variate.
  (setv k -1)
  (setv p 1)
  (while (> p (exp (- mean)))
    (+= k 1)
    (*= p (random.random)))
  k)

(defn 1-in [n]
  (chance (/ 1 n)))

(defn shuffle [l]
  (setv l (list l))
  (random.shuffle l)
  l)

(defn randpop [l]
  (l.pop (random.randrange (len l))))

(defn minsec [s]
  (.format "{}:{:02}" (// s 60) (% s 60)))

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
  (setv swapped False)
  (when (> p1.x p2.x)
    (setv [p1 p2] [p2 p1])
    (setv swapped True))
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
  (int (* 60 (+ 3 (/ dl 2)))))

(defn player? [cr]
  (is cr G.player))

(defn seen [pos]
  (get G.seen-map pos.x pos.y))

(defn msg [&rest args]
  (setv args (list args))
  (setv mtype (when (keyword? (first args))
    (.pop args 0)))
  (when mtype
    (setv (get args 0) (.format "{} {}"
      (color-xml
        (get {:tara "Tara:" :bob "Bob:" :aud "The audience"} mtype)
        (get G.announcer-colors mtype))
      (get args 0))))
  (.append G.message-log (,
    (len G.message-log)
    (apply .format args {"p" G.player}))))

(defn msgp [cr &rest args]
  (when (player? cr)
    (apply msg args)))

(defn color-xml [text &optional fg bg]
; Formats a colored string for AttrStr. (It should already be
; escaped for XML.)
  (if (or fg bg)
    (.format "<c{}{}>{}</c>"
      (if fg (.format " fg='{}'" (keyword->str fg)) "")
      (if bg (.format " bg='{}'" (keyword->str bg)) "")
      text)
    text))

(defn soil-fov []
  (setv G.fov-dirty? True))

(defn active-inv []
  (filt (.carry-effects-active? it) G.inventory))
