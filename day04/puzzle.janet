(def test-input ["MMMSXXMASM"
                 "MSAMXMSMSA"
                 "AMXSXMAAMM"
                 "MSAMASMSMX"
                 "XMASAMXAMM"
                 "XXAMMXXAMA"
                 "SMSMSASXSS"
                 "SAXAMASAAA"
                 "MAMMMXMMMM"
                 "MXMXAXMASX"])

(def xmas (string/bytes "XMAS"))
(def mas (string/bytes "MAS"))

(defn read-lines [path]
  (with [f (file/open "input")]
    (seq [line :in (file/lines f)] line)))

(defn line-segment [[x y] len degrees]
  (let [radians (* degrees (/ math/pi 180))
        dx (math/round (math/cos radians))
        dy (math/round (math/sin radians))]
    (map (fn [i] [(+ x (* i dx)) (+ y (* i dy))]) (range len))))

(defn neighbourhood [[i j] &opt len start by]
  (default len 4)
  (default start 0)
  (default by 45)
  (fiber/new
    (fn [] (loop [deg :range (start 360 by)] (yield (line-segment [i j] len deg))))))

(defn substring [grid line]
  (map |(get-in grid $) line))

(defn search [grid [i j]]
  (reduce (fn [aux line] (if (or (some neg? (last line))
                                 (some not= (substring grid line) xmas))
                           aux
                           (inc aux)))
          0 (neighbourhood [i j])))

(defn candidates [grid ch &opt len]
  (default len 4)
  (fiber/new
    (fn [] (loop [i :range (0 (length grid))]
             (each j (string/find-all ch (get grid i))
               (yield [i j]))))))

(defn puzzle1 [grid]
  (reduce (fn [aux pair] (+ aux (search grid pair))) 0 (candidates grid "X")))

(assert (= (puzzle1 test-input) 18))
(puzzle1 (read-lines "input"))

(defn diagonals [grid [i j]]
  (filter |(all = (substring grid $) mas) (neighbourhood [i j] 3 45 90)))

(defn intersections [xs]
  (->> (map |(get $ 1) xs)
       frequencies
       values
       (count |(> $ 1))))

(defn puzzle2 [grid]
  (->> (candidates grid "M")
       (mapcat (partial diagonals grid))
       intersections))

(assert (= (puzzle2 test-input) 9))
(puzzle2 (read-lines "input"))
