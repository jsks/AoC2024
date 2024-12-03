(defn read-lines [path]
  (with [f (file/open "input")]
    (seq [line :in (file/lines f)] line)))

(defn parse-report (str) (map scan-number (string/split " " (string/trim str))))

(defn conjoin
  "Boolean function combination"
  [& fns]
  (fn [& args] (every? (map (fn [pred] (pred ;args)) fns))))

(defn diff [xs]
  (map - (array/slice xs 0 -2) (array/slice xs 1)))

(defn between? [x]
  (let [abs-x (math/abs x)]
    (and (>= abs-x 1) (<= abs-x 3))))

(defn strict-safe? [report]
  (let [safe? (conjoin (if (pos? (- (first report) (last report))) pos? neg?) between?)]
    (all safe? (diff report))))

# This is such a stupid solution, but I don't want to spend any time
# on this puzzle.
(defn relative-safe? [report]
  (if-let [safe? (conjoin (if (pos? (- (first report) (last report))) pos? neg?) between?)
           idx (find-index (complement safe?) (diff report))]
    (or (all safe? (diff (array/remove (array/slice report) idx)))
        (all safe? (diff (array/remove report (inc idx)))))
    true))

(defn puzzle [input check?]
  (def reports (map parse-report input))
  (reduce (fn [aux x] (if (check? x) (inc aux) aux)) 0 reports))

(def test-input ["7 6 4 2 1"
                 "1 2 7 8 9"
                 "9 7 6 2 1"
                 "1 3 2 4 5"
                 "8 6 4 4 1"
                 "1 3 6 7 9"])

# Puzzle 1
(assert (= (puzzle test-input strict-safe?) 2))
(-> (read-lines "input") (puzzle strict-safe?) print)

# Puzzle 2
(assert (= (puzzle test-input relative-safe?) 4))
(-> (read-lines "input") (puzzle relative-safe?) print)
