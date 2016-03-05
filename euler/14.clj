; PROBLEM 14
; RESULT = 837799
; CODE:

(defn next-collatz
  [n]
  (cond
    (even? n) (quot n 2)
    (odd? n) (+ (* 3 n) 1)))

(defn collatz
  [starting]
  (concat
    (->>
      (iterate next-collatz starting)
      (take-while #(not (= % 1))))
      '(1)))

(defn longest-chain
  [limit]
  (loop [largest ()
         i 2
         m 2
         coll (collatz i)]
    (cond
      (= i (dec limit)) m
      (> (count coll) (count largest)) (recur coll (inc i) i (collatz (inc i)))
      :else (recur largest (inc i) m (collatz (inc i))))))

(println "Longest chain of Collatz from 13 =" (longest-chain 13))
(println "Longest chain of Collatz from 1M =" (longest-chain 1000000))
; (println "Longest Collatz under 1 million =" (longest-chain 1000000))
