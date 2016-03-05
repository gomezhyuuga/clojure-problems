; PROBLEM 16
; RESULT = 1366
; CODE:
(defn pow
  [a b]
  (->>
    (repeat b a)
    (reduce *)))
(defn sum-digits
  [n]
  (->>
    (str n)
    (map #(bigint (Character/digit % 10)))
    (reduce +)))
(println (sum-digits (pow 2 15)))
(println (sum-digits (pow 2N 1000N)))

