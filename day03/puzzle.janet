(def input (slurp "input"))
(def test-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn instruction [cmd & args] {:op cmd :args args})
(def pattn (peg/compile ~{:main (any (+ (/ (* :op "(" (at-most 1 :args) ")") ,instruction) 1))
                          :op '(+ "mul" "don't" "do")
                          :args (* (number (between 1 3 :d)) "," (number (between 1 3 :d)))}))

(defn puzzle1 [line]
  (->> (peg/match pattn line)
       (filter (fn [{:op op}] (= op "mul")))
       (reduce (fn [aux {:args xs}] (+ aux (product xs))) 0)))

(assert (= (puzzle1 test-input) 161))
(print (puzzle1 input))

(defn puzzle2 [line]
  (let [matches (peg/match pattn line)]
    (var state :exec)
    (var aux 0)
    (loop [{:op op :args args} :in matches]
      (case op
        "don't" (set state :ignore)
        "do"    (set state :exec)
        "mul"   (when (= state :exec) (+= aux (product args)))))
    aux))

(assert (= (puzzle2 test-input) 48))
(print (puzzle2 input))
