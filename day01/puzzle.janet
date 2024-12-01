(def test-input ["3 4"
                 "4 3"
                 "2 5"
                 "1 3"
                 "3 9"
                 "3 3"])

(defn parse-line [str] (map scan-number (peg/match ~{:main (* ':d+ :s+ ':d+)} str)))
(defn read-lines [path]
  (with [f (file/open "input")]
    (seq [line :in (file/lines f)] line)))

(defn puzzle [input proc]
  (def lines (map parse-line input))
  (let [a (map first lines)
        b (map last lines)]
    (proc a b)))

# Puzzle 1
(defn distance [a b]
  (sum (map (comp math/abs -) (sort a) (sort b))))

(assert (= (puzzle test-input distance) 11))
(-> (read-lines "input") (puzzle distance) print)

# Puzzle 2
(defn similarity [a b]
  (let [freq (frequencies b)]
    (reduce (fn [aux x] (+ aux (* x (get freq x 0)))) 0 a)))

(assert (= (puzzle test-input similarity) 31))
(-> (read-lines "input") (puzzle similarity) print)
