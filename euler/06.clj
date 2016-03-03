; PROBLEM 6
; RESULT = 25164150
; CODE:
(defn square-of-sum
  [limit]
  (let [sum (reduce + (range 1 (inc limit)))]
    (* sum sum)))
(defn sum-of-squares
  [limit]
  (->>
    (range 1 (inc limit))
    (map #(* % %))
    (reduce +)))
(defn euler-6
  [limit]
  (- (square-of-sum limit) (sum-of-squares limit)))
(println "Sum square difference w/ limit 10 = " (euler-6 10))
(println "Sum square difference w/ limit 100 = " (euler-6 100))
