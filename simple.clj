(use 'clojure.test)

;; FARENHEIT TO CELSIUS
(defn f2c
  "Takes x degrees farenheit and converts them to
  degrees Celsius."
  [x]
  (* (- x 32) 5/9))

;; SIGN EVAL
(defn sign
  "Returns -1 if n is negative,
  1 if n is positive greater than zero,
  or 0 if n is zero."
  [n]
  (if (= n 0) 0
  (if (> n 0) 1 -1)))

;; ROOTS OF AN EQ
(defn roots
  "Write a function called roots that returns a vector containing the two possible roots
  that solve a quadratic equation given its three coefficients (a, b, c) using the general
  formula"
  [a b c]
  (let [d (- b)
        e (Math/sqrt (- (* b b) (* 4 a c)))
        f (* 2 a)]
    [(/ (+ d e) f) (/ (- d e) f)]))

(defn bmi [weight height]
  (let [BMI (/ weight (* height height))]
    (cond
      (< BMI 20) 'underweight
      (< BMI 25) 'normal
      (< BMI 30) 'obese1
      (< BMI 40) 'obese2
      :default   'obese3)))

(defn factorial
  [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

;; TESTS
(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))
(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))
(deftest test-roots
  (is (= [-1.0 -1.0] (roots 2.0 4.0 2.0)))
  (is (= [0.0 0.0] (roots 1.0 0.0 0.0)))
  (is (= [-0.25 -1.0] (roots 4.0 5.0 1.0))))
(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))
(deftest test-factorial
         (is (= 1 (factorial 0)))
         (is (= 1 (factorial 1)))
         (is (= 2 (factorial 2)))
         (is (= 5 (factorial 120))))

(run-tests)

