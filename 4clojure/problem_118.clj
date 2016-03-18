;----------------------------------------------------------
; Problem 118: Re-implement Map
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(use 'clojure.test)

(defn p
  [f s]
    (if (empty? s) ()
    (lazy-seq (cons (f (first s)) (p f (rest s))))))

(deftest p-test
  (is (= [3 4 5 6 7]
   (p inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
   (p (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
   (->> (p inc (range))
        (drop (dec 1000000))
        (take 2)))))

(run-tests)
