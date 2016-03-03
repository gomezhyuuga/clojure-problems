; PROBLEM 9
; RESULT = 31875000
; CODE:
(defn pythagorean-triplet
  [limit]
  (for [a (range 1 limit)
        b (range 1 limit)
        c (range 1 limit)
        :when (and (< a b c)
                (= (* c c) (+ (* a a) (* b b))))]
    [a b c]))
; TODO: optimize
(defn euler-9
  []
  (->>
    (pythagorean-triplet 900)
    (filter (fn [[a b c]] (= 1000 (+ a b c))))
    (map #(vector % (reduce * %)))))
(time (println "Pythagorean triplet for which a + b + c = 1000, is " (euler-9)))

