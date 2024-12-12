(defn combinations [arr]
  (fiber/new (fn [] (loop [i :range (0 (dec (length arr)))
                           j :range ((inc i) (length arr))]
                      (yield [(arr i) (arr j)])))))

(defn locate [line column freq]
  {:row (dec line) :col (dec column) :freq freq})
(def pattn (peg/compile ~(any (+ (/ (* (line) (column) ':w) ,locate) 1))))

(defn gather [antennas]
  (let [aux @{}]
    (loop [x :in antennas]
      (if (aux (x :freq))
        (array/push (aux (x :freq)) [(x :col) (x :row)])
        (set (aux (x :freq)) @[[(x :col) (x :row)]])))
    aux))

(defn parse-antennas [str]
  (-> (peg/match pattn str) gather))

# Part 1
(defn antinodes [[a b]]
  (let [dy (- (b 1) (a 1))
        dx (- (b 0) (a 0))]
    [[(+ (b 0) dx) (+ (b 1) dy)] [(- (a 0) dx) (- (a 1) dy)]]))

(defn puzzle1 [input]
  (let [antennas (parse-antennas input)
        newlines (string/find-all "\n" input)
        nrow (length newlines)
        ncol (first newlines)]
    (->> (catseq [locations :in antennas]
                 (mapcat antinodes (combinations locations)))
         distinct
         (filter (fn [[x y]] (and (>= x 0) (< x ncol) (>= y 0) (< y nrow))))
         length)))

(def test-input (slurp "test"))
(assert (= (puzzle1 test-input) 14))

(def input (slurp "input"))
(pp (puzzle1 input))

# Part 2
(defn bounds [x low high delta]
  (let [kmin (/ (- low x) delta)
        kmax (/ (- high x) delta)]
    (if (neg? delta)
      [kmax kmin]
      [kmin kmax])))

(defn segment [[a b] upper lower]
  (let [dy (- (b 1) (a 1))
        dx (- (b 0) (a 0))
        d (math/gcd (math/abs dx) (math/abs dy))
        xbounds (bounds (a 0) (lower 0) (upper 0) (/ dx d))
        ybounds (bounds (a 1) (lower 1) (upper 1) (/ dy d))]
    (seq [k :range-to ((math/ceil (max (xbounds 0) (ybounds 0)))
                        (math/floor (min (xbounds 1) (ybounds 1))))
          :let [node [(+ (a 0) (* k (/ dx d))) (+ (a 1) (* k (/ dy d)))]]]
      node)))

(defn puzzle2 [input]
  (let [antennas (parse-antennas input)
        newlines (string/find-all "\n" input)
        upper [(dec (first newlines)) (dec (length newlines))]]
    (-> (catseq [locations :in antennas]
                (mapcat |(segment $ upper [0 0]) (combinations locations)))
        distinct
        length)))

(def test-input (slurp "test"))
(assert (= (puzzle2 test-input) 34))

(def input (slurp "input"))
(pp (puzzle2 input))
