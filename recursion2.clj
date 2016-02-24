;----------------------------------------------------------
; Activity: Recursive Functions, Part II
; Date: February 4, 2016.
; Authors:
;          A01020319 Fernando Gómez herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn my-repeat
  "Takes a number n and any data x as its arguments. It returns a list that
  contains n copies of x. Do not use the predefined repeat function."
  [n x]
  (loop [result () counter n]
    (if (zero? counter)
      result
      (recur (cons x result) (dec counter)))))
(defn invert-pairs
  "Takes as an argument a list of vectors containing two elements each. It
  returns a new list with every vector pair inverted"
  [lst]
  (loop [result ()
         coll lst
         vect (first coll)]
    (if (empty? coll)
      result
      (let [frst  (first vect)
            scnd  (second vect)
            pair  (list [scnd frst])
            r     (rest coll)
            v     (first r)]
        (recur (concat result pair) r v)))))
(defn enlist
  "Surrounds in a list every upper-level element of the list it takes as input."
  [lst]
  (loop [res () coll lst]
    (if (empty? coll)
      res
      (recur (concat res (list (list (first coll)))) (rest coll)))))
(defn my-interleave
  "Takes two arguments: the lists a and b. It returns a list containing the
  first element of a, followed by the first element of b, followed by the second
  element of a, followed by the second element of b, and so on.
  The lists a and b don't have to be of the same size. The interleaving of the
  elements stops when either a or b is exhausted. Do not use the predefined
  interleave function."
  [a b]
  (loop [result () lst1 a lst2 b]
    (cond
      (empty? lst1) result
      (empty? lst2) result
      :else (recur (concat result (list (first lst1)
                   (first lst2)))
                   (rest lst1) (rest lst2)))))
(defn my-flatten
  "Removes all the interior parenthesis of the list it takes as input. Do not
  use the predefined flatten function."
  [lst]
  (loop [result () coll lst]
    (if (empty? coll)
      (reverse result)
      (let [el (first coll)]
      (if (seq? el)
        (recur result (concat el (rest coll)))
        (recur (cons el result) (rest coll)))))))
(defn exchange
  "Takes three arguments: two non-list values x1 and x2, and a list lst. It
  returns a list with the same elements as lst, except that all occurrences of
  x1 are replaced by x2 and vice versa, including any occurrences inside nested
  lists."
  [x1 x2 lst]
  (if (empty? lst)
    ()
    (cond
      (= (first lst) x1) (cons x2 (exchange x1 x2 (rest lst)))
      (= (first lst) x2) (cons x1 (exchange x1 x2 (rest lst)))
      :else (cons (first lst) (exchange x1 x2 (rest lst))))))
(defn insert
  "Takes two arguments: a number n and a list of numbers lst in ascending order.
  It returns a new list with the same elements as lst but inserting n in its
  corresponding place."
  [n lst]
  (if (empty? lst)
    (cons n lst)
    (let [f (first lst) r (rest lst)]
      (if (> f n)
        (cons n lst)
        (cons f (insert n r))))))
(defn my-sort
  "Takes an unordered list of numbers as an argument, and returns a new list
  with the same elements but in ascending order. You must use the insert
  function defined in the previous exercise to write the my-sort."
  [lst]
  (if (empty? lst)
    ()
    (insert (first lst) (my-sort (rest lst)))))
(defn binary
  "Takes an integer n as input (assume that n ≥ 0). If n is equal to zero, it
  returns an empty list. If n is greater than zero, it returns a list with a
  sequence of ones and zeros equivalent to the binary representation of n."
  [n]
  (loop [res () num n]
    (if (zero? num)
      res
      (recur (cons (rem num 2) res) (quot num 2)))))
(defn prime-factors
  "Takes an integer n as input (assume that n > 0), and returns a list
  containing the prime factors of n in ascending order. The prime factors are
  the prime numbers that divide a number exactly. If you multiply all the prime
  factors you get the original number."
  [n]
  (if (< n 2)
    ()
    (loop [res () factor 2 num n]
      (let [r (rem num factor)
            d (quot num factor)]
        (if (= num 1)
          (reverse res)
          (if (= r 0)
            (recur (cons factor res) factor d)
            (recur res (inc factor) num)))))))
(defn compress
  "Takes a list lst as its argument. If lst contains consecutive repeated
  elements, they should be replaced with a single copy of the element. The order
  of the elements should not be changed."
  [lst]
  (loop [res () coll lst]
    (if (empty? coll)
      (reverse res)
      (if (= (first res) (first coll))
        (recur res (rest coll))
        (recur (cons (first coll) res) (rest coll))))))
(defn pack
  "Takes a list lst as its argument. If lst contains consecutive repeated
  elements they should be placed in separate sublists."
  [lst]
  (partition-by identity lst))
(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst
  are encoded as vectors [n e], where n is the number of duplicates of the
  element e"
  [lst]
  (->>
    (pack lst)
    (map #(vector (count %) (first %)))))
(defn encode-modified
  "Takes a list lst as its argument. It works the same as the previous problem,
  but if an element has no duplicates it is simply copied into the result list."
  [lst]
  (->>
    (encode lst)
    (map (fn [[c, e]] (if (= c 1) e [c, e])))))
(defn decode
  "Takes as its argument an encoded list lst that has the same structure as the
  resulting list from the previous problem. It returns the decoded version of lst."
  [lst]
  (mapcat (fn [x] (if (vector? x)
                 (repeat (x 0) (x 1))
                 (list x))) lst))
; TESTS
(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
    (exchange true 42 '((true) 42 ((cool (42)) (true))))))
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
    (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
    (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
    (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
    (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
