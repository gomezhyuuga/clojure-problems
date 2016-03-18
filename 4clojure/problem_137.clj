;----------------------------------------------------------
; Problem 137: Digits and bases
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [n b]
  (letfn [(f [n b]
            (let [r (rem n b)]
              (if (zero? (quot n b))
                (vector r)
                (conj (p (quot n b) b) r))))]
     (f n b)))

(deftest p-test
  (is (= [1 2 3 4 5 0 1] (p 1234501 10)))
  (is (= [0] (p 0 11)))
  (is (= [1 0 0 1] (p 9 2)))
  (is (= [1 0] (let [n (rand-int 100000)](p n n))))
  (is (= [16 18 5 24 15 1] (p Integer/MAX_VALUE 42))))

(run-tests)
