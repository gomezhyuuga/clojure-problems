; PROBLEM 48
; RESULT = 9110846700
; CODE:
(defn pow
  [a b]
  (->>
    (repeat b a)
    (reduce *)))
(defn euler-48
  [limit]
  (->>
    (range 1N (inc limit))
    (map #(pow % %))
    (reduce +)
    str
    reverse
    (reduce str)))
(println "Euler 48 = " (reduce str (reverse (take 10 (euler-48 1000)))))
