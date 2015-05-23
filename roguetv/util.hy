(import
  [math [sqrt]]
  random
  [heidegger.pos [Pos]]
  [kodhy.util [seq]]
  [roguetv.globals :as G])

(defn chance [x]
  (<= (random.random) x))

(defn 1-in [n]
  (chance (/ 1 n)))

(defn shuffle [l]
  (setv l (list l))
  (random.shuffle l)
  l)

(defn pick [l]
  (first (random.sample l 1)))

(defn randpop [l]
  (l.pop (random.randrange (len l))))

(defn minsec [s]
  (.format "{}:{:02}" (// s 60) (% s 60)))

(defn show-round [number ndigits]
  (setv x (round number ndigits))
  (string (if (= x (int x)) (int x) x)))

(defn len-taxicab [p]
  ; The length of a vector according to the taxicab norm (1-norm).
  ; So Pos.ORTHS have length 1 and Pos.DIAGS have length 2.
  ; In Rogue TV, taxicab geometry is the rule.
  (+ (abs p.x) (abs p.y)))
(defn dist-taxicab [p1 p2]
  (len-taxicab (- p1 p2)))

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

(defn player? [cr]
  (is cr G.player))

(defn msg [&rest args]
  (setv args (list args))
  (setv mtype (when (keyword? (first args))
    (.pop args 0)))
  (.append G.message-log (,
    (len G.message-log)
    mtype
    (apply .format args {"p" G.player}))))

(defn msgp [cr &rest args]
  (when (player? cr)
    (apply msg args)))

(defn soil-fov []
  (setv G.fov-dirty? True))
