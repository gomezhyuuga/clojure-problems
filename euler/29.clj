; PROBLEM 29
; RESULT = 9183
; CODE:
(defn pow
  [[a b]]
  (->>
    (repeat b a)
    (reduce *)))
(defn combinations
  [[start end]]
  (for [i (range start (inc end))
        j (range start (inc end))]
    [i j]))
(defn terms
  [[start end]]
  (->>
    (map pow (combinations [start end]))
    distinct
    sort
    count))

(println "Distinct powers [2, 5] =" (terms [2N 5N]))
(println "Distinct powers [2, 100] =" (terms [2N 100N]))
