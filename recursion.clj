;----------------------------------------------------------
; Activity: Recursive Functions, Part I
; Date: January 28, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use 'clojure.test)

(defn my-count
  "Returns the number of elements contained in its input list. Do not use
  the predefined count function."
  [coll]
  (if (empty? coll)
    0
    (+ 1 (my-count (rest coll)))))

(defn add-list
  "Returns the sum of all the elements of its input list, or 0 if its empty.
  Assume that all the elements in the input list are numbers."
  [coll]
  (if (empty? coll)
    0
    (+ (first coll) (add-list (rest coll)))))
(defn member?
  "Takes two arguments, any data x and a list lst. Returns true if x is
  contained in lst, false otherwise."
  [x lst]
  (if (empty? lst)
    false
    (if (= x (first lst))
      true
      (member? x (rest lst)))))
(defn list-of-symbols?
  "Takes a list lst as its argument. It returns true if all the elements
  (possibly zero) contained in lst are symbols, or false otherwise. Use the
  symbol? predicate to determine if something is a symbol."
  [lst]
  (if (empty? lst)
    true
    (if (symbol? (first lst))
      (list-of-symbols? (rest lst))
      false)))
(defn my-last
  "The function my-last returns the last element of its input list, or nil
  of its empty. Do not use the predefined last function."
  [coll]
  (if (empty? coll)
    nil
    (if (= (rest coll) '())
      (first coll)
      (my-last (rest coll)))))
(defn cons-end
  "The function cons-end takes two arguments, any data x and a list lst.
  Returns a list composed by the same elements of lst but with x at the end."
  [x lst]
  (if (empty? lst)
    (cons x ())
    (cons (first lst) (cons-end x (rest lst)))))
(defn my-butlast
  "Returns a list with the same elements as its input list but excluding the
  last element, or nil of its empty."
  [lst]
  (if (empty? lst)
    nil
    (if (empty? (rest lst))
      ()
      (cons (first lst) (my-butlast (rest lst))))))
(defn my-reverse
  "Takes a list as an argument. It returns another list with the same
  elements as the input list, but in reverse order."
  [lst]
  (if (empty? lst)
    ()
    (cons (my-last lst) (my-reverse (my-butlast lst)))))
(defn my-concat
  "Returns the resulting list of appending the two lists it takes as input."
  [lst1 lst2]
  (if (empty? lst1)
    lst2
    (if (empty? lst2)
      lst1
      (cons (first lst1) (my-concat (rest lst1) lst2)))))
(defn deep-reverse
  "Takes a list as its input. It returns a list with the same elements as its
  input but in reverse order. If there are any nested lists, these too should
  be reversed."
  [coll]
  (if (empty? coll)
    ()
    (if (coll? (my-last coll))
      (cons (deep-reverse (my-last coll)) (deep-reverse (my-butlast coll)))
      (cons (my-last coll) (deep-reverse (my-butlast coll))))))

(defn largest
  "Returns the largest number in the list"
  [lst]
  (loop [largest (first lst) (rest lst)]
    (if (empty? lst)
      largest
      (if (> (first lst) largest)
        (recur (first lst) (rest lst))
        (recur largest (rest lst))))))

; TESTS
(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))
(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))
(deftest test-member?
    (is (not (member? 'a ())))
    (is (member? 'a '(a b c)))
    (is (member? 'a '(c b a b c)))
    (is (not (member? 'x '(a b c)))))
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))
(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))
(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))
(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))
(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))
(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))
(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))

(run-tests)
