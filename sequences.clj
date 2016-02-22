;----------------------------------------------------------
; Activity: Using the Sequence API
; Date: February 25, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(use 'clojure.test)

(defn positives
  "Takes a list of numbers lst as its argument, and returns a new list that
  only contains the positive numbers of lst."
  [lst]
  (filter pos? lst))
(defn dot-product
  "Takes two arguments: the lists a and b. It returns the result of performing
  the dot product of a times b."
  [a b]
  (cond
    (empty? a) 0
    (empty? b) 0
    :else (reduce + (map (fn [one two] (* one two)) a b))))

(defn pow
  "Takes two arguments as input: a number a and a positive integer b. It returns
  the result of computing a raised to the power b."
  [a b]
  (reduce * (repeat b a)))

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))
(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))
(run-tests)
