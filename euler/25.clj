; PROBLEM 25
; RESULT = 4782
; CODE:
(defn fibo
  "Returns a lazy collection with all the fibonacci numbers."
  []
  (map-indexed (fn [index el] [(inc index) (first el)]) (iterate (fn [[l n]] [n (+ l n)]) [1N 1N])))
(defn fibos
  [n]
  (->>
    (fibo)
    (drop-while (fn [[index number]] (< (count (str number)) n)))
    first))

(println "Fibo 3 digits =" (fibos 3))
(println "Fibo 1000 digits =" (fibos 1000))
