(defn by-line [str]
  (string/split "\n" str))

(defn has? (arr x)
  (some |(= x $) arr))

(defn intersects? [a b]
  (some |(has? a $) b))

(defn split-rule [str]
  (map scan-number (string/split "|" str)))

(defn parse-rules [header]
  (let [rule-pairs (map split-rule (by-line header))]
    (def rules @{})
    (loop [[a b] :in rule-pairs
           :before (when (nil? (rules a)) (set (rules a) @[]))]
      (array/push (rules a) b))
    rules))

(defn split-pages [str]
  (->> (string/split "," str)
       (map scan-number)
       reverse))

(defn parse-page-list [block]
  (->> (filter (complement empty?) (by-line block))
       (map split-pages)))

(defn parse [input]
  (let [[header block] (string/split "\n\n" (slurp input))]
    {:rules (parse-rules header)
     :page-lists (parse-page-list block)}))

(defn valid? [pages rules]
  (match pages
    [head & rest] (unless (intersects? (get rules head @[]) rest)
                    (valid? rest rules))
    _ true))

(defn middle [pages]
  (get pages (div (length pages) 2)))

# Part 1
(defn puzzle1 [{:rules rules :page-lists page-lists}]
  (->> (filter |(valid? $ rules) page-lists)
       (reduce (fn [aux pages] (+ aux (middle pages))) 0)))

(def test-input (parse "test"))
(def input (parse "input"))

(assert (= (puzzle1 test-input) 143))
(print (puzzle1 input))

# Part 2
(defn intersect-at [a b]
  (find |(has? a (b $)) (range (length b))))

(defn swap! [arr i j]
  (let [tmp (arr i)]
    (set (arr i) (arr j))
    (set (arr j) tmp)))

(defn fix! [pages rules &opt i]
  (default i 0)
  (when (< i (dec (length pages)))
    (let [idx (intersect-at (get rules (pages i) @[]) (array/slice pages (inc i)))]
      (if (nil? idx)
        (fix! pages rules (inc i))
        (do
          (swap! pages i (+ i idx 1))
          (fix! pages rules i))))))

(defn puzzle2 [{:rules rules :page-lists page-lists}]
  (let [invalid-pages (filter |(not (valid? $ rules)) page-lists)]
    (each pages invalid-pages (fix! pages rules))
    (reduce (fn [aux pages] (+ aux (middle pages))) 0 invalid-pages)))

(assert (= (puzzle2 test-input) 123))
(print (puzzle2 input))
