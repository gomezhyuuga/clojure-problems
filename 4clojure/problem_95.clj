;----------------------------------------------------------
; Problem 95: To Tree, or not to Tree
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(use 'clojure.test)

(defn p
  [s]
  (if-not (= 3 (count s))
    false
    (let [left (nth s 1)
          right (nth s 2)]
      (cond
        (and (coll? left) (coll? right)) (and (p left) (p right))
        (coll? left) (p left)
        (coll? right) (p right)
        (false? left) false
        (false? right) false
        :else true))))


(deftest test-p
  (is (= (p '(:a (:b nil nil) nil))
         true))
  (is (= (p '(:a (:b nil nil)))
         false))
  (is (= (p [1 nil [2 [3 nil nil] [4 nil nil]]])
         true))
  (is (= (p [1 [2 nil nil] [3 nil nil] [4 nil nil]])
         false))
  (is (= (p [1 [2 [3 [4 nil nil] nil] nil] nil])
         true))
  (is (= (p [1 [2 [3 [4 false nil] nil] nil] nil])
         false))
  (is (= (p '(:a nil ()))
         false)))


(run-tests)
