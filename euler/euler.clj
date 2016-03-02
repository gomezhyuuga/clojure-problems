(use 'clojure.test)
; HELPER functions
(defn pow
  [a b]
  (->>
    (repeat b a)
    (reduce *)))
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

; EULER 5
(defn euler-5-example
  [limit]
  (let [numbers (range 1 limit)]
    (->>
      (iterate inc limit)
      (map (fn [el]
             [el (every? #(zero? (rem el %)) numbers)]))
      (drop-while (fn [[n, t]] (false? t)))
      first
      first)))
(println "Smallest number that can be divided by each N in 1-10 = " (euler-5-example 11))
(println "Smallest number that is evenly divisible by each N in 1-20 = " (euler-5-example 21))

; EULER 48
(defn euler-48
  [limit]
  (->>
    (range 1N (inc limit))
    (map #(pow % %))
    (reduce +)
    str
    reverse))
(println "Euler 48 EX = " (euler-48 10))
(println "Euler 48 = " (reverse (take 10 (euler-48 1000))))

; EULER 17
(defn element-to-word
  "Returns the readable word representation of a number."
  [n]
  (condp = n
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    7 "seven"
    8 "eight"
    9 "nine"
    10 "ten"
    11 "eleven"
    12 "twelve"
    13 "thirteen"
    14 "fourteen"
    15 "fifteen"
    16 "sixteen"
    17 "seventeen"
    18 "eighteen"
    19 "nineteen"
    20 "twenty"
    30 "thirty"
    40 "forty"
    50 "fifty"
    60 "sixty"
    70 "seventy"
    80 "eighty"
    90 "ninety"))
(defn number-to-word
  [n]
  (let [thousands (quot n 1000)
        trem      (rem n 1000)
        hundreds  (quot trem 100)
        hrem      (rem n 100)
        dozens    (quot n 10)])
  [
   ])

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
