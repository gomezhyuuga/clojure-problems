; PROBLEM 3
; RESULT = 6857
; Largest prime factor of 600851475143
(defn prime?
  "Naive implementation for checking if a number n is prime or not."
  [n]
  (loop [i 2]
    (cond
      (> i (Math/sqrt n)) true
      (= (rem n i) 0) false
      :else (recur (inc i)))))
(defn primes
  "Returns a lazy collection of prime numbers."
  [limit]
  (filter prime? (range 2 (inc limit))))

(defn prime-factors
  [n]
  (let [factors (primes n)]
    (->>
      (filter #(zero? (rem n %)) factors)
      last)))
(defn factors
  [n]
  (loop [largest 2 i 2]
    (if (> i (Math/sqrt n))
      largest
      (if (and (prime? i) (zero? (rem n i)))
        (recur i (inc i))
        (recur largest (inc i))))))

(println "Largest prime factor of 13195 = " (factors 13195))
(time
(println "Largest prime factor of 13195 = " (factors 600851475143)))

