(declare prime? primes)
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

(defn sum-of-primes
  [limit]
  (reduce + (take-while #(< % limit) (primes limit))))

(println "Sum of primes below 10 =" (sum-of-primes 10))
(println "Sum of primes below 2 million =" (sum-of-primes 2000000))

