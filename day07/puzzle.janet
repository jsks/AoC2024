(defn read-lines [path]
  (drop -1 (string/split "\n" (slurp path))))

(def- pattn (peg/compile ~{:arg (* :s+ (number :d+))
                           :main (* (number :d+) ":" (some :arg))}))
(defn parse-line [line] (peg/match pattn line))

(defn eval-op [op x y]
  (case op
    'mul (* x y)
    'add (+ x y)
    'cat (scan-number (string x y))))

(defn traverse [target ops aux [x & rest]]
  (catseq [op :in ops]
          (let [aux (eval-op op aux x)]
            (cond
              (and (= target aux) (empty? rest)) true
              (or (> aux target) (empty? rest)) false
              (traverse target ops aux rest)))))

(defn search [line &named all-ops]
  (let [[y & x] (parse-line line)
        ops (if (true? all-ops) '(add mul cat) '(add mul))]
    (if (pos? (count truthy? (traverse y ops (first x) (drop 1 x))))
      y
      0)))

###
# Test
(def test-input ["190: 10 19"
                 "3267: 81 40 27"
                 "83: 17 5"
                 "156: 15 6"
                 "7290: 6 8 6 15"
                 "161011: 16 10 13"
                 "192: 17 8 14"
                 "21037: 9 7 18 13"
                 "292: 11 6 16 20"])

(assert (= (sum (map search test-input)) 3749))
(assert (= (sum (map |(search $ :all-ops true) test-input)) 11387))

###
# Main
(def input (read-lines "input"))

# Part 1
(sum (map search input))

# Part 2
(sum (map |(search $ :all-ops true) input))
