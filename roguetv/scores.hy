(require [kodhy.macros [lc amap afind ecase block λ]])

(import
  [math [floor ceil]]
  [itertools [groupby]]
  json
  errno
  [decimal [Decimal]]
  [kodhy.util [T F ret ucfirst keyword->str]]
  [roguetv.english [english-list]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [text-screen]])

(defn get-scores [path]
  (try
    (with [o (open path "r" :encoding "UTF-8")]
      (setv scores (get (json.load o) "scores")))
    (except [e IOError]
      (unless (= e.errno errno.ENOENT)
        (raise))
      (setv scores [])))
  scores)

(defn add-current-game-to-scores [path prizes gross]
  (setv x (dict
    :name (.format "{:a}" G.player)
    :dates G.dates
    :seeds G.seeds
    :bundle-os G.bundle-os
    :bundle-git G.bundle-git
    :dungeon-level G.dungeon-level
    :time G.current-time
    :endgame (keyword->str G.endgame)
    :gross gross
    :prizes (lc [g (amap (list (second it)) (groupby prizes (λ (type it))))]
      (if (= (len g) 1)
        (.format "{:a:true}" (first g))
        (.format "{} {:num:true}" (len g) (first g))))))
  (setv scores (get-scores path))
  (.append scores x)
  ; Sort the highest-scoring characters first, breaking ties with
  ; newer characters first.
  (.sort scores :reverse T :key (λ (,
    (get it "gross")
    (get it "dates" "ended"))))
  (with [o (open path "w" :encoding "UTF-8")]
    (json.dump {"scores" scores} o :sort-keys T)))

(defn show-scores [path &optional [show-all F]]

  (setv accum [""])
  (defn out [&rest args]
    (+= (get accum 0) (+
      (if args (.format (first args) #* (rest args)) "")
      "\n")))
  (setv scores (get-scores path))

  (block
    (unless (len scores)
      (out "There aren't any scores to show.")
      (ret))

    (out "Games: {}" (len scores))
    (out "Mean score: <b>${}</b>"
      (show-round :ndigits 2 (/
        (sum (amap (get it "gross") scores))
        (len scores))))
    (out)

    (setv latest (max scores :key (λ (get it "dates" "ended"))))

    (when (or (< (len scores) 3) show-all)
      (for [character scores]
        (out (show-character character latest)))
      (ret))

    (defn print-latest []
      (out "<b>• Last score</b> ({} quantile)" (no-leading-0
        (round :ndigits 3 (- 1 (/ (inc (.index scores latest)) (len scores))))))
      (out (show-character latest latest)))
    (setv printed-latest F)

    (setv low-quantile (/ (- 1 (Decimal G.score-interval)) 2))
    (setv high-quantile (- 1 low-quantile))
    (for [[text q f] [
        ["High score" high-quantile floor]
        ["Median score" .5 round]
        ["Low score" low-quantile ceil]]]
      (setv target-gross (get scores
        (min (dec (len scores)) (int (f (* (len scores) (- 1 q)))))
        "gross"))
      (setv character (afind (= (get it "gross") target-gross) scores))
      (when (and (not printed-latest) (> (get latest "gross") (get character "gross")))
        (print-latest)
        (setv printed-latest T))
      (out (.format "<b>• {}</b> ({} quantile)" text (no-leading-0 q)))
      (when (= character latest)
        (out "is also the last score")
        (setv printed-latest T))
      (out (show-character character latest)))

    (unless printed-latest
      (print-latest)))

  (text-screen (first accum)))

(defn show-character [x &optional latest]
  (setv [d1 d2] (amap
    (.lstrip (.strftime
        (datetime.datetime.strptime
          (cut (get x "dates" it) 0 (len "2004-12-31"))
          "%Y-%m-%d")
        "%d %b %Y")
      "0")
    ["started" "ended"]))
  (.join "\n" [
    (.format "<b>{}</b> ({})"
      (color-xml (ucfirst (get x "name"))
        :bg (when (and latest (= x latest)) :yellow))
      (if (= d1 d2)
        d1
        (+ d1 " – " d2)))
    (.format "{} level {} {}"
      (ecase (get x "endgame")
        ["won"              "won the game on"]
        ["out-of-time"      "ran out of time on"]
        ["resigned"         "resigned on"]
        ["used-up-elevator" "took the elevator up from"])
      (inc (get x "dungeon_level"))
      (if (< (get x "time") G.clock-factor)
        "in less than a second"
        (+ "after " (show-duration (get x "time")
          :trunc-to-sec T :abbreviate T))))
    (if (get x "prizes")
      (.format "with {} worth <b>${}</b>: {}."
        (if (= (len (get x "prizes")) 1) "a prize" "prizes")
        (get x "gross")
        (english-list (get x "prizes")))
      "empty-handed.")
    ""]))

(defn no-leading-0 [x]
  (if (< 0 x 1)
    (cut (string x) 1)
    (string x)))
