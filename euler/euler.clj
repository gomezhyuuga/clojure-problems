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

; PROBLEM 1
(defn multiples
  "Returns a lazy collection with all the multiples of a certain number."
  [n]
  (iterate #(+ n %) n))
(defn euler-1
  "Sums all the multiples of a and b below limit."
  [a b limit]
  (->> (range 1 limit)
    (filter #(or (zero? (mod % a)) (zero? (mod % b))))
    (reduce +)))
(println "Multiples of 3 5 below 1000 = " (euler-1 3 5 1000))

; PROBLEM 2
; fibo function taken from the book
(defn fibo
  "Returns a lazy collection with all the fibonacci numbers."
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
(defn prime-factors
  [n]
  (->>
    (for [i (range 2 (Math/sqrt n)) :when (prime? i)] i)
    (filter #(zero? (rem n %)))
    last))
(println "Largest prime factor of 13195 = " (prime-factors 13195))

; PROBLEM 7
; NTH prime number
(defn prime-nth
  "Get the nth prime number."
  [n]
  (->>
    (primes)
    (take n)
    last))
; PROBLEM 10
(defn sum-primes
  "Sum all the prime numbers below limit."
  [limit]
  (->>
    (take-while #(< % limit) (primes))
    (reduce +)))
; (println "Sum of primes below 2M = " (sum-primes 2000000))

; EULER PROBLEM 4
(defn palindrome?
  [s]
  (let [size (count s)
        half (quot size 2)]
    (if (= size 1)
      true
      (->>
        (for [i (range 0 half)
              :while (= (nth s i) (nth s (- (dec size) i)))] i)
        last
        (= (dec half))))))
(defn largest
  "Takes as argument a nonempty list of numbers lst. It returns the largest
  value contained in lst. Use the reduce function to solve this problem."
  [lst]
  (reduce (fn [a b] (if (> a b) a b)) lst))
(defn palindrome-product
  [a b]
  (->>
    (for [i (reverse (range a b)) j (range a b)] (* i j))
    (filter (fn [el] (palindrome? (str el))))
    largest))
(println "Largest palindrome of two digits = " (palindrome-product 10 100))
(println "Largest palindrome of three-digits = " (palindrome-product 100 1000))

; EULER 6
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

; EULER 9
(defn pythagorean-triplet
  [limit]
  (for [a (range 1 limit)
        b (range 1 limit)
        c (range 1 limit)
        :when (and (< a b c)
                (= (* c c) (+ (* a a) (* b b))))]
    [a b c]))
(defn euler-9
  []
  (->>
    (pythagorean-triplet 900)
    (filter (fn [[a b c]] (= 1000 (+ a b c))))))
(println "Pythagorean triplet for which a + b + c = 1000, is " (euler-9))

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
  (is (= 31875000 (euler-9)) "EULER 9") ; EULER 9
)
; (run-tests)
