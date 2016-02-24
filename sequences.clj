;----------------------------------------------------------
; Activity: Using the Sequence API
; Date: February 25, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(use 'clojure.test)

(defn positives
  "Takes a list of numbers lst as its argument, and returns a new list that
  only contains the positive numbers of lst."
  [lst]
  (filter pos? lst))
(defn dot-product
  "Takes two arguments: the lists a and b. It returns the result of performing
  the dot product of a times b."
  [a b]
  (cond
    (empty? a) 0
    (empty? b) 0
    :else (reduce + (map (fn [one two] (* one two)) a b))))

(defn pow
  "Takes two arguments as input: a number a and a positive integer b. It returns
  the result of computing a raised to the power b."
  [a b]
  (reduce * (repeat b a)))

(defn replic
  "takes two arguments: a list lst and an integer number n, where n ≥ 0. It
  returns a new list that replicates n times each element contained in lst."
  [n lst]
  (mapcat (fn [el] (repeat n el)) lst))
(defn expand
  "Takes a list lst as its argument. It returns a list where the first element
  of lst appears one time, the second elements appears two times, the third
  element appears three times, and so on."
  [lst]
  (mapcat identity (map-indexed (fn [index item] (repeat (inc index) item)) lst)))
(defn largest
  "Takes as argument a nonempty list of numbers lst. It returns the largest
  value contained in lst. Use the reduce function to solve this problem."
  [lst]
  (reduce (fn [a b] (if (> a b) a b)) lst))
(defn drop-every
  "Takes two arguments: an integer number n, where n ≥ 1, and a list lst. It
  returns a new list that drops every n-th element from lst."
  [n lst]
  (remove list? (map-indexed (fn [index item] (if (not (zero? (rem (inc index) n))) item ())) lst)))
(defn rotate-left
  "Takes two arguments: an integer number n and a list lst. It returns the list
  that results from rotating lst a total of n elements to the left. If n is
  negative, it rotates to the right."
  [n lst]
  (if (empty? lst)
    ()
    (let [rotations (mod n (count lst))]
      (concat (drop rotations lst) (take rotations lst)))))
(defn gcd
  "Takes two positive integer arguments a and b as arguments, where a > 0 and b
  > 0. It returns the greatest common divisor (GCD) of a and b."
  [a b]
    (let [n (min a b)]
      (->>
        (iterate (fn [[f, s, n]] [f, s, (dec n)]) [a, b, n])
        (map (fn [[f, s, n]] [(+ (rem f n) (rem s n)), n]))
        (drop-while (fn [[g, n]] (not (zero? g))))
        first
        second)))
(defn insert-everywhere
  "Takes two arguments as input: an object x and a list lst. It returns a new
  list with all the possible ways in which x can be inserted into every position
  of lst"
  [x lst]
  (->>
    (range 0 (inc (count lst)))
    (map #(let [[t h] (split-at % lst)]
            (concat t (list x) h)))))

(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4) '(-4.5 3.0 1.5 0.9 0.0)))))
(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))
(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4) (replic 4 '(1 2 3 4)))))
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e) (expand '(a b c d e)))))
(deftest test-largest
  (is (= 31 (largest '(31))))
  (is (= 5 (largest '(1 2 3 4 5))))
  (is (= -1 (largest '(-1 -2 -3 -4 -5))))
  (is (= 52 (largest '(32 -1 45 12 -42 52 17 0 21 2)))))
(deftest test-drop-every
  (is (= () (drop-every 5 ())))
  (is (= '(1 2 3) (drop-every 4 '(1 2 3 4))))
  (is (= '(1 3 5 7) (drop-every 2 '(1 2 3 4 5 6 7 8))))
  (is (= '(1 3 5 7 9) (drop-every 2 '(1 2 3 4 5 6 7 8 9))))
  (is (= '(a b d e g h j) (drop-every 3 '(a b c d e f g h i j))))
  (is (= '(a b c d e f g h i j)
         (drop-every 20 '(a b c d e f g h i j))))
  (is (= () (drop-every 1 '(a b c d e f g h i j)))))
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))
(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))
(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
          (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
          (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(run-tests)
