;----------------------------------------------------------
; Activity: MiniKanren
; Date: May 2, 2016.
; Authors:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use '[clojure.core.logic :rename {is logic-is}])
(use 'clojure.test)

(defn remove
  [x lst]
  (cond
    (= x (first lst)) (rest lst)
    :else (cons (first lst) (remove x (rest lst)))))

(defne removeo
  "This logic function succeeds if it’s able to remove the first occurrence of
  x from lst giving result."
  [x lst result]
  ([x [x . t] t])
  ([_ [h . t] _]
   (fresh [tmp]
          (removeo x t tmp)
          (appendo [h] tmp result))))
(declare even-sizeo odd-sizeo)
(defne even-sizeo
  "These two mutually recursive logic functions succeed if the number of
  elements in lst is even or odd, respectively."
  [lst]
  ([ [] ]  succeed)
  ([ [h] ] fail)
  ([ [h . t] ]
   (odd-sizeo t)))
(defne odd-sizeo
  "These two mutually recursive logic functions succeed if the number of elements
  in lst is even or odd, respectively."
  [lst]
  ([ [] ]  fail)
  ([ [h] ] succeed)
  ([ [h . t] ]
   (even-sizeo t)))

(defne reverseo
  [lst result]
  ([[] []])
  ([[h . t] result]
   (fresh [x]
          (reverseo t x)
          (appendo x [h] result))))
(defne palindromeo
  "This logic function succeeds if lst is a palindrome list (it reads the same
  from left to right than from right to left)."
  [lst]
  ([_]
   (fresh [tmp]
          (reverseo lst tmp)
          (== tmp lst))))

(deftest test-removeo
  (is (= [[:b :c :d :e]]
         (run 1 [q] (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
         (run 1 [q] (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
         (run 1 [q] (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
         (run 1 [q] (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e]
          [:a :b :x :c :d :e]
          [:a :b :c :x :d :e]
          [:a :b :c :d :x :e]
          [:a :b :c :d :e :x]]
         (run 6 [q] (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]]
          [:b [:a :c :d :e]]
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
         (run* [q1 q2]
               (removeo q1 [:a :b :c :d :e] q2)))))
(deftest test-even-sizeo-odd-sizeo
  (is (= [:yes]
         (run 1 [q] (even-sizeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (odd-sizeo [:x]) (== q :yes))))
  (is (= []
         (run 1 [q] (even-sizeo [:x]) (== q :yes))))
  (is (= []
         (run 1 [q] (odd-sizeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (even-sizeo [:a :b :c :d :e :f]) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (odd-sizeo [:a :b :c :d :e]) (== q :yes))))
  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (run 5 [q] (even-sizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (run 5 [q] (odd-sizeo q)))))

(deftest test-palindromeo
  (is (= [:yes]
         (run 1 [q] (palindromeo []) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (palindromeo [:a]) (== q :yes))))
  (is (= [:yes]
         (run 1 [q] (palindromeo [:a :b :c :b :a]) (== q :yes))))
  (is (= []
         (run 1 [q] (palindromeo [:a :b :c :d]) (== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (run 7 [q] (palindromeo q)))))

  (run-tests)
