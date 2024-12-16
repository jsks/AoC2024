(defn subst [& xs] (tabseq [[id a b] :in xs] (keyword id) [a b]))
(def pattn (peg/compile ~{:main (some (+ (/ (* (group :entry) :s* (group :entry)) ,subst) "\n"))
                          :pair (* (number (* (any "-") :d+)) ","
                                   (number (* (any "-") :d+)))
                          :entry (* (<- :a) "=" :pair)}))

(defn predict! [robot ncol nrow]
  (let [{:v [dx dy]} robot]
    (update robot :p (fn [[x y]] [(% (+ (+ x dx) ncol) ncol) (% (+ (+ y dy) nrow) nrow)]))))

(defn quadrant [[x y] ncol nrow]
  (let [cols (div ncol 2)
        rows (div nrow 2)]
    (cond
      (and (< x cols) (< y rows)) 1
      (and (< x cols) (> y rows)) 2
      (and (> x cols) (< y rows)) 3
      (and (> x cols) (> y rows)) 4)))

(defn safety [robots ncol nrow]
  (-> (map |(quadrant (get $ :p) ncol nrow) robots)
      frequencies
      product))

(defn puzzle1 [robots ncol nrow]
  (repeat 100 (each robot robots (predict! robot ncol nrow)))
  (safety robots ncol nrow))

(defn min-index-of [xs]
  (extreme (fn [x y] (< (get x 1) (get y 1))) (pairs xs)))

(defn puzzle2 [robots ncol nrow]
  (def scores (seq [i :range (0 10_403)]
                (each robot robots (predict! robot ncol nrow))
                (safety robots ncol nrow)))
  (inc (first (min-index-of scores))))

(def test-input (peg/match pattn (slurp "test")))
(assert (= (puzzle1 test-input 11 7) 12))

(def input (peg/match pattn (slurp "input")))
(puzzle1 input 101 103)

(def input (peg/match pattn (slurp "input")))
(puzzle2 input 101 103)
