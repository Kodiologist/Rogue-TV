(import
  random
  [roguetv.globals :as G])

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

(defn msg [mtype &rest format-args]
  (.append G.message-log (,
    (len G.message-log)
    mtype
    (apply .format format-args {"p" G.player}))))

(defn msgn [&rest format-args]
  (apply msg (+ (, None) format-args)))
