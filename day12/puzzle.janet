(defn parse-map [file]
  (->> (slurp file) string/trim (string/split "\n")))

(defn move [coord direction]
  (let [[i j] coord]
    (case direction
      :N [(dec i) j]
      :E [i (inc j)]
      :S [(inc i) j]
      :W [i (dec j)])))

(defn navigate [grid target coord region]
  (def edges {:N @[] :E @[] :S @[] :W @[]})
  (defn recurse [coord]
    (put region coord true)
    (loop [direction :in [:N :E :S :W]
           :let [next (move coord direction)]
           :unless (get region next)]
      (if (not= (get-in grid coord) (get-in grid next))
        (array/push (edges direction) next)
        (recurse next))))
  (recurse coord)
  edges)

(defn unique-sides [edges]
  (def visited @{})
  (defn visit-edges [coord]
    (put visited coord true)
    (loop [direction :in [:N :E :S :W]
           :let [next (move coord direction)]
           :when (and (not (get visited next)) (index-of next edges))]
      (visit-edges next)))

  (var aux 0)
  (loop [coord :in edges :unless (get visited coord)]
    (visit-edges coord)
    (++ aux))
  aux)

(defn prices [grid &named discount]
  (def visited @{})
  (def count-fn (if (truthy? discount) unique-sides length))
  (seq [i :range (0 (length grid)) j :range (0 (length (first grid)))
        :let [coord [i j] region @{}]
        :unless (get visited coord)]
    (let [boundaries (navigate grid (get-in grid coord) coord region)]
      (merge-into visited region)
      (* (sum (map count-fn boundaries)) (length region)))))

(def puzzle (comp sum prices))

(def test-input (parse-map "test"))
(assert (= (puzzle test-input) 1930))
(assert (= (puzzle test-input :discount true) 1206))

(def input (parse-map "input"))
(pp (puzzle input))
(pp (puzzle input :discount true))
