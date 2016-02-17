(use 'clojure.test)

(defn add-list
  "Returns the sum of all the elementes of its input list or 0 if its empty"
  [lst]
  (reduce + lst))
(defn list-of-symbol
  [lst]
  (every? symbol? lst))
(defn invert-pairs
  [lst]
  (map (fn [[a b]] [b a]) lst))
(defn enlist
  "Surrounds in a list every upper-level element of the list it takes as input."
  [lst]
  (map list lst))
(defn insert
  "Takes two arguments: a number n and a list of numbers lst in ascending order.
  It returns a new list with the same elements as lst but inserting n in its
  corresponding place."
  [n lst]
  (concat (take-while #(< % n) lst)
          (list n)
          (drop-while #(< % n) lst)))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 4 5 6 7 8) (insert 4 '(4 5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))
(run-tests)
