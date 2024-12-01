(def input ["3 4"
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
  (let [a (map |(get $ 0) lines)
        b (map |(get $ 1) lines)]
    (proc a b)))

# Puzzle 1
(defn distance [a b]
  (sum (map |(math/abs (- $0 $1)) (sort a) (sort b))))

(assert (= (puzzle input distance) 11))
(-> (read-lines "input") (puzzle distance) print)

# Puzzle 2
(defn similarity [a b]
  (reduce (fn [aux x] (+ aux (* (count |(= $ x) b) x))) 0 a))

(assert (= (puzzle input similarity) 31))
(-> (read-lines "input") (puzzle similarity) print)
