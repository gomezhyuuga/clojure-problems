; PROBLEM 5
; RESULT = 232792560
; CODE:
(defn euler-5
  [limit]
  (let [numbers (range 1 limit)]
    (->>
      (iterate inc limit)
      (map (fn [el]
             [el (every? #(zero? (rem el %)) numbers)]))
      (drop-while (fn [[n, t]] (false? t)))
      first
      first)))
(println "Smallest number that can be divided by each N in 1-10 = " (euler-5 11))
(println "Smallest number that is evenly divisible by each N in 1-20 = " (euler-5 21))
