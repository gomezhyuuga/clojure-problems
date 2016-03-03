(use 'clojure.test)
; HELPER functions
(defn prime?
  "Naive implementation for checking if a number n is prime or not."
  [n]
  (loop [i 2]
    (cond
      (= i n) true
      (= (rem n i) 0) false
      :else (recur (inc i)))))
(defn primes
  "Returns a lazy collection of prime numbers."
  []
  (filter prime? (iterate inc 2)))

(deftest test-helpers
  (is (= true (prime? 2)))
  (is (= true (prime? 3)))
  (is (= false (prime? 4)))
  (is (= true (prime? 19)))
  (is (= true (prime? 29)))
  (is (= false (prime? 30)))
  (is (= 29 (prime-factors 13195))))

(deftest test-answers
  (is (= 23 (euler-1 3 5 10)) "EULER 1 EXAMPLE")
  (is (= 233168 (euler-1 3 5 1000)) "EULER 1")
  (is (= 4613732N (euler-2 4000000N)) "EULER 2") ; ANSWER 1
  (is (= 104743 (prime-nth 10001)) "EULER 7") ; EULER 7
  (is (= 17 (sum-primes 10)) "EULER 10 EXAMPLE")
  (is (= 9009 (palindrome-product 10 100)) "EULER 4 EXAMPLE")
  (is (= 906609 (palindrome-product 100 1000)) "EULER 4") ; EULER 4
  (is (= 2640 (euler-6 10)) "EULER 6 EXAMPLE")
  (is (= 25164150 (euler-6 100)) "EULER 6") ; EULER 6
  (is (= 232792560 (euler-5)) "EULER 5") ; EULER 5
)
; (run-tests)
