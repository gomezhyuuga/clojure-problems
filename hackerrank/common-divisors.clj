(use 'clojure.test)

(defn divisors
  [n]
  (concat (for [i (range 1 (inc (quot n 2)))
                :when (zero? (rem n i))] i) (list n)))
;(defn common-divisors
;[m l]
;(let [div_m (set (divisors m))
;div_l (set (divisors l))]
;(count (clojure.set/intersection div_m div_l))))
(defn common-divisors
  [m l]
  (let [limit (inc (quot (min m l) 2))
        divisors (for [i (range 1 limit)
                       :when (and (zero? (rem m i)) (zero? (rem l i)))] i)]
    (if (zero? (rem (max m l) (min m l)))
      (inc (count divisors))
      (count divisors))))


;(doseq [line (rest (line-seq (java.io.BufferedReader. *in*)))]
;(let [[m l] (map read-string (re-seq #"\d+" line))]
;(println (common-divisors m l))))

(deftest test-divisors
  (is (= '(1) (divisors 1)))
  (is (= '(1 2 5 10) (divisors 10)))
  (is (= '(1 2 4) (divisors 4)))
  (is (= '(1 2 4 5 10 20) (divisors 20)))
  (is (= 20 (count (divisors 93250113)))))

(deftest test-common-divisors
  (is (= 128 (common-divisors 9699690 510510)))
  (is (= 20 (common-divisors 93250113 93250113)))
  (is (= 2 (common-divisors 10 4)))
  (is (= 1 (common-divisors 1 100)))
  (is (= 10 (common-divisors 288 240))))

(time (run-tests))
