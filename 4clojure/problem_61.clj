;----------------------------------------------------------
; Problem 61: Map Construction
; Date: March 17, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn p
  [coll1 coll2]
  (->>
    (map #(array-map %1 %2) coll1 coll2)
    (reduce merge)))

(deftest p-test
  (is (= (p [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (p [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (p [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(run-tests)
