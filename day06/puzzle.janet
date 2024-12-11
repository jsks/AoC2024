(defn read-lines [path]
  (with [f (file/open path)]
    (seq [line :in (file/lines f)] (string/trim line))))

(defn between? [x a b]
  (and (>= x a) (< x b)))

(def directions {:N [-1 0] :E [0 1] :S [1 0] :W [0 -1]})

(defn turn-right [direction]
 (case direction
  :N :E
  :E :S
  :S :W
  :W :N))

(defn move [[i j] direction]
  (let [[x y] (directions direction)]
    [(+ i x) (+ j y)]))

(defn inbounds [grid [i j]]
  (and (between? i 0 (length grid))
       (between? j 0 (length (first grid)))))

(defn at [grid [i j]]
  (when (inbounds grid [i j])
    ((grid i) j)))

(def Guard
  @{:start [0 0]
    :pos [0 0]
    :direction :N
    :visited @{}
    :reset (fn [self]
             (set (self :pos) (self :start))
             (set (self :direction) :N)
             (set (self :visited) @{}))
    :walk (fn [self grid]
            (let [next-pos (move (self :pos) (self :direction))]
              (if  (= (at grid next-pos) 35)
                (set (self :direction) (turn-right (self :direction)))
                (do
                  (update (self :visited) [next-pos (self :direction)]
                          |(if (nil? $) 1 (inc $)))
                  (set (self :pos) next-pos))))) })

(defn find-start [grid &opt row]
  (default row 0)
  (if-let [col (string/find "^" (grid row))]
    [row col]
    (find-start grid (inc row))))

(defn make-guard [grid]
  (let [start (find-start grid)]
    (table/setproto @{:start start :pos start :visited @{}} Guard)))

(defn puzzle1 [input]
  (let [guard (make-guard input)]
    (while (inbounds input (guard :pos))
      (:walk guard input))

    (->> (keys (guard :visited))
         (map (fn [[pos _]] pos))
         distinct)))

###
# Part 2
(defn flip! [grid [i j]]
  (set ((grid i) j) (if (= ((grid i) j) 35) 46 35)))

(defmacro with-flip! [grid pos & body]
  ~(defer (flip! ,grid ,pos)
    (flip! ,grid ,pos)
    ,;body))

(defn simulate [grid guard]
  (:walk guard grid)
  (cond
    (not (inbounds grid (guard :pos))) false
    (> (get (guard :visited) [(guard :pos) (guard :direction)] 0) 1) true
    (simulate grid guard)))

(defn find-loops [grid candidates]
  (def guard (make-guard grid))
  (seq [pos :in candidates
        :when (and (not= pos (guard :start))
                   (inbounds grid pos))]
    (:reset guard)
    (with-flip! grid pos (simulate grid guard))))

(defn puzzle2 [input candidates]
    (count truthy? (find-loops input candidates)))

###
# Test
(def test-input (thaw (read-lines "test")))
(def squares (puzzle1 test-input))

(assert (= (dec (length squares)) 41))
(assert (= (puzzle2 test-input squares)) 6)

###
# Main
(def input (thaw (read-lines "input")))
(def squares (puzzle1 input))

# Part 1
(pp (length squares))

# Part 2
(pp (puzzle2 input squares))
