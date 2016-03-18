;----------------------------------------------------------
; Problem 135: Infix Calculator
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [& args]
  (let [num1 (nth args 0)
        num2 (nth args 2)
        op   (nth args 1)
        expr (list op num1 num2)
        value (eval expr)]
    (if (= 3 (count args))
      value
      (apply p (cons value (drop 3 args))))))

(deftest test-p
  (is (= 7  (p 2 + 5)))
  (is (= 42 (p 38 + 48 - 2 / 2)))
  (is (= 8  (p 10 / 2 - 1 * 2)))
  (is (= 72 (p 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(run-tests)
