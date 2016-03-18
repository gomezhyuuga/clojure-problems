;----------------------------------------------------------
; Problem 99: Product Digits
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [n1 n2]
	(map #(Character/digit % 10) (seq (str (* n1 n2)))))

(deftest test-p
         (is (= (p 1 1) [1]))
         (is (= (p 99 9) [8 9 1]))
         (is (= (p 999 99) [9 8 9 0 1])))

(run-tests)
