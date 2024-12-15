(defn second [xs] (get xs 1))

(defn integer? [x] (zero? (% x 1)))

(def blocks (string/split "\n\n" (slurp "test")))

(defn collate [id x y] {(keyword id) [x y]})
(def pattn
  (peg/compile ~{:main (some (+ :prize :button "\n"))
                 :prize (/ (* (<- "Prize") ":" :s* "X=" (number :d+) "," :s* "Y=" (number :d+)) ,collate)
                 :button (/ (* "Button" :s* '(range "AZ") ":" :s* :coord) ,collate)
                 :coord (* "X+" (number :d+) "," :s* "Y+" (number :d+))}))

(defn parse-machines [str]
  (map |(merge ;(peg/match pattn $)) (string/split "\n\n" str)))

(defn bump-prize [{:Prize prize :A a :B b}]
  {:Prize [(+ 10000000000000 (first prize)) (+ 10000000000000 (second prize))] :A a :B b})

(defn optim [prize a b]
  (def slope (/ (- (* 3 (first b)) (first a)) (first b)))
  (defn recurse [x &opt prev]
    (let [y (/ (- (second prize) (* (first a) x)) (first b))]
      (cond
        (neg? y) prev
        (and (integer? y) (neg? slope)) (recurse (inc x) [x y])
        (and (integer? y) (pos? slope)) [x y]
        (recurse (inc x)))))
  (recurse 0))

(defn solve [{:Prize prize :A a :B b}]
  (let [det (- (* (first a) (second b)) (* (second a) (first b)))]
    (if (not= det 0)
      (let [x (/ (- (* (first prize) (second b)) (* (second prize) (first b))) det)
            y (/ (- (* (second prize) (first a)) (* (first prize) (second a))) det)]
        (when (and (integer? x) (integer? y))
          [x y]))
      (optim prize a b))))

(defn cost [[x y]] (+ (* x 3) y))

(defn puzzle [input]
  (->> (map solve input)
       (filter (complement nil?))
       (map cost)
       sum))

(def test-input (parse-machines (slurp "test")))
(assert (= (puzzle test-input) 480))

# Part 1
(def input (parse-machines (slurp "input")))
(pp (puzzle input))

# Part 2
(pp (puzzle (map bump-prize input)))
