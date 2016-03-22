(use 'clojure.test)

(defn divisors
  [n]
  (for [i (range 1 (+ 2 (quot n 2)))
        :when (zero? (rem n i))] i))
(defn common-divisors
  [m l]
  (let [div_m (set (divisors m))
        div_l (set (divisors l))]
    (count (clojure.set/intersection div_m div_l))))

(deftest test-divisors
  (is (= '(1) (divisors 1)))
  (is (= '(1 2 5) (divisors 10)))
  (is (= '(1 2) (divisors 4)))
  (is (= '(1 2 4 5 10) (divisors 20))))

(deftest test-common-divisors
  (is (= 2 (common-divisors 10 4)))
  (is (= 1 (common-divisors 1 100)))
  (is (= 10 (common-divisors 288 240))))

(run-tests)
