;----------------------------------------------------------
; Problem 60: Sequence Reductions
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  ([f s]
   (p f (first s) (rest s)))
  ([f i s]
   (if (empty? s)
     (list i)
     (lazy-seq (cons i (p f (f i (first s)) (rest s)))))))

(deftest p-test
  (is (= (take 5 (p + (range))) [0 1 3 6 10]))
  (is (= (p conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (is (= (last (p * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))

(run-tests)
