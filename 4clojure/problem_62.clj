;----------------------------------------------------------
; Problem 62: Re-implement Iterate
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)
(defn p
  [f x]
  (lazy-seq (cons x (p f (f x)))))

(deftest p-test
  (is (= (take 5 (p #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (p inc 0)) (take 100 (range))))
  (is (= (take 9 (p #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))


(run-tests)
