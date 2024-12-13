# Part 1
(def input (string/trim (slurp "input")))

(def sequence (peg/match ~(some (number :d)) input))
(def files (seq [i :range (0 (length input)) :when (even? i)] (sequence i)))

(def forward-queue (seq [[i x] :in (reverse (pairs files)) :repeat x] i))
(def backward-queue (seq [[i x] :pairs files :repeat x] i))

(def aux (sum files))
(var len 0)
(def gen (generate [[i x] :pairs sequence
                          :repeat x
                          :after (++ len)
                          :until (= len aux)]
           (* len (if (even? i) (array/pop forward-queue) (array/pop backward-queue)))))
(sum gen)

# Part 2
(defn entry [i n]
  @{:id (when (even? i) (/ i 2)) :space n :file (even? i)})
(def sequence (peg/match ~(some (+ "0" (/ (* ($) (number (range "19"))) ,entry))) input))

(var pos 0)
(loop [entry :in sequence
             :after (+= pos (entry :space))]
  (set (entry :position) pos))

(def diskmap (tabseq [entry :in sequence] (entry :position) entry))

# This is a sloppy solution, but got to catch up.
(defn find-empty-space [diskmap required-space max-slot]
  (find (fn [slot] (and (not (nil? (diskmap slot)))
                        (not (get-in diskmap [slot :file]))
                        (>= (get-in diskmap [slot :space]) required-space)))
        (range 0 max-slot)))

(loop [slot :in (reverse (sort (keys diskmap)))
            :when (get-in diskmap [slot :file])]
  (when-let [empty-slot (find-empty-space diskmap (get-in diskmap [slot :space]) slot)
             remaining-space (- (get-in diskmap [empty-slot :space] 0)
                                (get-in diskmap [slot :space]))]
    (set ((diskmap slot) :position) (get-in diskmap [empty-slot :position]))
    (if (= remaining-space 0)
      (set (diskmap empty-slot) nil)
      (do (+= ((diskmap empty-slot) :position) (get-in diskmap [slot :space]))
        (set ((diskmap empty-slot) :space) remaining-space)))))

(defn checksum [entry]
  (+ (* (entry :space) (entry :id) (entry :position))
     (/ (* (entry :id) (entry :space) (dec (entry :space))) 2)))

(sum (keep |(and ($ :file) (checksum $)) diskmap))
