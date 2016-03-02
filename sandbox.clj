(use 'clojure.test)
(defn next-pentagon
  [n]
  (/ (* n (- (* 3 n) 1)) 2))
(defn pentagonals
  []
  (map second (iterate (fn [[n, p]] [(inc n) (next-pentagon (inc n))]) [1 1])))

(defn pentagonal?
  [n]
  (= 0.0
  (let [sq (Math/sqrt (+ 1 (* 24 n)))]
        (rem (+ 1 sq) 6))))

(defn euler
  [limit]
  (let [coll (take limit (pentagonals))]
  (for [j coll
        k coll
        :when (and
               (pentagonal? (+ j k))
               (pentagonal? (- k j)))]
    [j k])))


(println (euler 100))

(deftest test-pentagonal
  []
  (is (= true (pentagonal? 1)))
  (is (= true (pentagonal? 5)))
  (is (= true (pentagonal? 330)))
  (is (= true (pentagonal? 287)))
  (is (= true (pentagonal? 477)))
  (is (= false (pentagonal? 2)))
  (is (= false (pentagonal? 10)))
  (is (= false (pentagonal? 40)))
         )

(defn problem10
  [n]
  (->> lazy-primes
   (take-while #(< % n))
  (reduce +)))
(problem10 20)

(defn prime?
  [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

(def lazy-primes
  (filter #(prime? %)  (iterate inc 2)))
(defn pow
  [a b]
  (->>
    (repeat b a)
    (reduce *)))
(pow
(defn euler-48
  [limit]
  (->>
    (range 1 (inc limit))
    (map #(* % %))
    (reduce +)))
(println "Euler 48 " (euler-48 10))
(run-tests)
