; PROBLEM 1
; RESULT =
; CODE:
(defn multiples
  "Returns a lazy collection with all the multiples of a certain number."
  [n]
  (iterate #(+ n %) n))
(defn euler-1
  "Sums all the multiples of a and b below limit."
  [a b limit]
  (->> (range 1 limit)
    (filter #(or (zero? (mod % a)) (zero? (mod % b))))
    (reduce +)))
(println "Multiples of 3 5 below 1000 = " (euler-1 3 5 1000))
