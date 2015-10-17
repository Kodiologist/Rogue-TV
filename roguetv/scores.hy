(require kodhy.macros)

(import
  [math [floor ceil]]
  [itertools [groupby]]
  json
  errno
  [kodhy.util [ret ucfirst keyword->str]]
  [roguetv.english [english-list]]
  [roguetv.globals :as G]
  [roguetv.util [*]]
  [roguetv.input [text-screen]])

(defn get-scores [path]
  (try
    (with [[o (open path "rb")]]
      (setv scores (get (json.load o) "scores")))
    (catch [e OSError]
      (unless (= e.errno errno.EEXIST)
        (raise))
      (setv scores [])))
  scores)

(defn add-current-game-to-scores [path prizes gross]
  (setv x (kwc dict
    :name (.format "{:a}" G.player)
    :dates G.dates
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
  (kwc .sort scores :key (λ (,
    (get it "gross")
    (get it "dates" "ended"))))
  (with [[o (open path "wb")]]
    (kwc json.dump {"scores" scores} o :+sort-keys)))

(defn show-scores [path &optional [show-all False]]

  (setv accum [""])
  (defn out [&rest args]
    (+= (get accum 0) (+ (apply .format (or args [""])) "\n")))
  (setv scores (get-scores path))

  (block
    (unless (len scores)
      (out "There aren't any scores to show.")
      (ret))

    (out "Games: {}" (len scores))
    (out "Mean score: <b>${}</b>"
      (kwc show-round :ndigits 2 (/
        (sum (amap (get it "gross") scores))
        (len scores))))
    (out)

    (setv latest (kwc max scores :key (λ (get it "dates" "ended"))))

    (when (or (< (len scores) 3) show-all)
      (for [character (reversed scores)]
        (out (show-character character latest)))
      (ret))

    (defn print-latest []
      (out "<b>• Last score</b> ({} quantile)" (no-leading-0
        (kwc round :ndigits 3 (/ (inc (.index scores latest)) (len scores)))))
      (out (show-character latest latest)))
    (setv printed-latest False)

    (setv low-quantile (/ (- 1 G.score-interval) 2))
    (setv high-quantile (- 1 low-quantile))
    (for [[text q f] [
        ["High score" high-quantile ceil]
        ["Median score" .5 round]
        ["Low score" low-quantile floor]]]
      (setv target-gross (get scores
        (min (dec (len scores)) (int (f (* (len scores) q))))
        "gross"))
      (setv character (get (filt (= (get it "gross") target-gross) scores) -1))
      (when (and (not printed-latest) (> (get latest "gross") (get character "gross")))
        (print-latest)
        (setv printed-latest True))
      (out (.format "<b>• {}</b> ({} quantile)" text (no-leading-0 q)))
      (when (= character latest)
        (out "is also the last score")
        (setv printed-latest True))
      (out (show-character character latest)))

    (unless printed-latest
      (print-latest)))

  (text-screen (first accum)))

(defn show-character [x &optional latest]
  (setv [d1 d2] (amap
    (.lstrip (.strftime
        (datetime.datetime.strptime
          (slice (get x "dates" it) 0 (len "2004-12-31"))
          "%Y-%m-%d")
        "%d %b %Y")
      "0")
    ["started" "ended"]))
  (.join "\n" [
    (.format "{} ({})"
      (kwc color-xml (ucfirst (get x "name"))
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
        (+ "after " (hour-min-sec-elapsed (get x "time")))))
    (if (get x "prizes")
      (.format "with {} worth <b>${}</b>: {}."
        (if (= (len (get x "prizes")) 1) "a prize" "prizes")
        (get x "gross")
        (english-list (get x "prizes")))
      "empty-handed.")
    ""]))

(defn no-leading-0 [x]
  (if (< 0 x 1)
    (slice (string x) 1)
    (string x)))
