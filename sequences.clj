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

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))

(run-tests)
