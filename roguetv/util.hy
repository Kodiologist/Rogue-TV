(import
  random
  [kodhy.util [seq]]
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

(defn ray-taxicab [
    start     ; Pos
    direction ; Pos
    length]   ; int
  (setv l [start])
  (for [_ (range (dec (if (in direction Pos.DIAGS) (// length 2) length)))]
    (.append l (+ (get l -1) direction)))
  l)

(defn disc-taxicab [
    center  ; Pos
    radius] ; int
  (list-comp (+ center (Pos dx dy)) [
    dx (seq (- radius) radius)
    dy (seq (- (abs dx) radius) (- radius (abs dx)))]))

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
