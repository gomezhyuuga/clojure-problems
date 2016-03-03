; PROBLEM 7
; RESULT = 104743
; NTH prime number
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
  []
  (filter prime? (iterate inc 2)))
(defn prime-nth
  "Get the nth prime number."
  [n]
  (nth (primes) (dec n)))

(println "6th prime =" (prime-nth 6))
(println "1001th prime =" (prime-nth 10001))
