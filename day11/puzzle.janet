(defn parse-stones [str]
  (peg/match ~(some (+ (number :d+) :s)) str))

(defn ndigits [x] (math/floor (inc (math/log10 x))))

(defn split-digits [x]
  (let [k (math/pow 10 (/ (ndigits x) 2))]
    [(div x k) (% x k)]))

(defn blink! [stones]
  (loop [[mark n] :pairs (table/clone stones)
                  :when (not= n 0)]
    (-= (stones mark) n)
    (cond
      (= mark 0) (put stones 1 (+ (get stones 1 0) n))
      (even? (ndigits mark)) (let [[a b] (split-digits mark)]
                               (put stones a (+ (get stones a 0) n))
                               (put stones b (+ (get stones b 0) n)))
      (put stones (* mark 2024) (+ (get stones (* mark 2024) 0) n)))))

(defn puzzle [stones iter]
  (let [tbl (tabseq [stone :in stones] stone 1)]
    (loop [i :range (0 iter)]
      (blink! tbl))
    (sum (values tbl))))

(def test (parse-stones "125 17"))
(assert (= (puzzle test 6) 22))
(assert (= (puzzle test 25) 55312))

(def input (parse-stones (slurp "input")))
(puzzle input 25)
(puzzle input 75)
