(use 'clojure.test)
; PROBLEM 1
(defn multipliers
  [n]
  (iterate #(+ n %) n))
(defn multiples
  [a b limit]
  (->> (range 1 limit)
    (filter #(or (zero? (mod % a)) (zero? (mod % b))))
    (reduce +)))
(println "Multiples of 3 5 below 1000 = " (multiples 3 5 1000))

; PROBLEM 2
; fibo function taken from the book
(defn fibo
  []
  (map first (iterate (fn [[l n]] [n (+ l n)]) [1N 2N])))
(defn euler-2 [limit]
  (->>
    (fibo)
    (take-while #(< % limit))
    (filter even?)
    (reduce +)))
(println "Sum of fibo even < four million = " (euler-2 4000000N))

; PROBLEM 3
; Largest prime factor of 600851475143
(defn prime?
  [n]
  (loop [i 2]
    (cond
      (= i n) true
      (= (rem n i) 0) false
      :else (recur (inc i)))))
(defn prime-factors
  [n]
  (->>
    (range 2 n)
    (map (fn [el] [el (rem n el)]))
    (filter (fn [[i, r]] (and (zero? r) (prime? i))))
    (take-while (fn [[i, _]] (< i (Math/sqrt n))))
    last
    first))
(println "Largest prime factor of 13195 = " (prime-factors 13195))
; (println "Largest prime factor of 600851475143 = " (prime-factors 600851475143))

(deftest test-helpers
  (is (= true (prime? 2)))
  (is (= true (prime? 3)))
  (is (= false (prime? 4)))
  (is (= true (prime? 19)))
  (is (= true (prime? 29)))
  (is (= false (prime? 30))))

(deftest test-answers
  (is (= 23 (multiples 3 5 10)))
  (is (= 233168 (multiples 3 5 1000)))
  (is (= 4613732N (euler-2 4000000N)))) ; ANSWER 1
(run-tests)
