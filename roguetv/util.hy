(import
  random
  [heidegger.pos [Pos]]
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
; p should be a Pos.
  (+ (abs p.x) (abs p.y)))

(defn ray [start direction length]
  ; The length of the ray is measured according to taxicab distance.
  (setv l [start])
  (for [_ (range (dec (if (in direction Pos.DIAGS) (// length 2) length)))]
    (.append l (+ (get l -1) direction)))
  l)

(defn msg [&rest args]
  (setv args (list args))
  (setv mtype (when (keyword? (first args))
    (.pop args 0)))
  (.append G.message-log (,
    (len G.message-log)
    mtype
    (apply .format args {"p" G.player}))))
