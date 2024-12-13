(defn second [xs] (get xs 1))

(defn parse-input [file]
  (peg/match ~(any (+ (group (some (number :d))) :s)) (slurp file)))

(defn find-all-in [arr x]
  (catseq [row :range (0 (length arr))]
          (seq [col :range (0 (length (arr row)))
                    :when (= (get-in arr [row col]) x)]
            [row col])))

(defn navigate [grid coord &opt aux]
  (default aux @[])
  (let [next-height (inc (get-in grid coord))]
    (loop [[i j] :in [[0 -1] [-1 0] [0 1] [1 0]]
                 :let [next [(+ (first coord) i) (+ (second coord) j)]]
                 :when (and (>= (first next) 0) (>= (second next) 0)
                            (= next-height (get-in grid next)))]
      (if (= next-height 9)
        (array/push aux next)
        (navigate grid next aux))))
  aux)

(defn find-all-trails [grid]
    (map |(navigate grid $) (find-all-in grid 0)))

(defn puzzle [file]
  (let [grid (parse-input file)
        trails (find-all-trails grid)]
    [(sum (map (comp length distinct) trails)) (sum (map length trails))]))

(def [test-part1 test-part2] (puzzle "test"))
(assert (= test-part1 36))
(assert (= test-part2 81))

(printf "Part 1: %d, Part 2: %d" ;(puzzle "input"))
