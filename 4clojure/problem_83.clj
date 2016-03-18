;----------------------------------------------------------
; Problem 83: A Half-Truth
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [& args]
  (and (not (not-any? true? args)) (not (every? true? args))))

(deftest p-test
  (is (= false (p false false)))
  (is (= true (p true false)))
  (is (= false (p true)))
  (is (= true (p false true false)))
  (is (= false (p true true true)))
  (is (= true (p true true true false))))

(run-tests)
