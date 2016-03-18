;----------------------------------------------------------
; Problem 63: Group a Sequence
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [f s]
  (loop [res {} coll s]
    (if (empty? coll)
      res
      (let [el (first coll)
            r (f el)]
        (recur (merge-with concat res {r (vector el)}) (rest coll))))))

(deftest p-test
  (is (= (p #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (p #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (p count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))
(run-tests)
