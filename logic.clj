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
(defne rotateo
  "This logic function succeeds when lst is rotated left one position giving
  result. In other words, the first element of lst becomes the last element of result."
  [lst result]
  ([[h . t] _]
   (appendo t [h] result)))
(defne converto
  "This logic function succeeds when digit d corresponds to the keyword k (for
  example digit 7 with keyword :seven)."
  [d k]
  ([0 :zero] succeed)
  ([1 :one] succeed)
  ([2 :two] succeed)
  ([3 :three] succeed)
  ([4 :four] succeed)
  ([5 :five] succeed)
  ([6 :six] succeed)
  ([7 :seven] succeed)
  ([8 :eight] succeed)
  ([9 :nine] succeed)
  ([_ _] fail))

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
(deftest test-rotateo
  (is (= [:yes]
         (run 1 [q]
              (rotateo [:a :b :c :d :e]
                       [:b :c :d :e :a])
              (== q :yes))))
  (is (= []
         (run 1 [q]
              (rotateo [:a :b :c :d :e]
                       [:a :b :c :d :e])
              (== q :yes))))
  (is (= []
         (run 1 [q] (rotateo [] q))))
  (is (= [[:a]]
         (run 1 [q] (rotateo [:a] q))))
  (is (= [[:b :c :d :e :a]]
         (run 1 [q] (rotateo [:a :b :c :d :e] q))))
  (is (= [[:e :a :b :c :d]]
         (run 1 [q] (rotateo q [:a :b :c :d :e]))))
  (is (= '[[[_0] [_0]]
           [[_0 _1] [_1 _0]]
           [[_0 _1 _2] [_1 _2 _0]]
           [[_0 _1 _2 _3] [_1 _2 _3 _0]]
           [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
           [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
           [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
         (run 7 [q1 q2] (rotateo q1 q2)))))
(deftest test-converto
  (is (= [:yes]
         (run 1 [q] (converto 0 :zero) (== q :yes))))
  (is (= [:yes]
         (run 1 [q]
              (converto 0 :zero)
              (converto 1 :one)
              (converto 2 :two)
              (converto 3 :three)
              (converto 4 :four)
              (converto 5 :five)
              (converto 6 :six)
              (converto 7 :seven)
              (converto 8 :eight)
              (converto 9 :nine)
              (== q :yes))))
  (is (= []
         (run 1 [q] (converto 2 :one) (== q :yes))))
  (is (= []
         (run 1 [q] (converto 12 :twelve) (== q :yes))))
  (is (= [7]
         (run 1 [q] (converto q :seven))))
  (is (= [:seven]
         (run 1 [q] (converto 7 q))))
  (is (= [[1 :two 3]]
         (run 1 [q1 q2 q3]
              (converto q1 :one)
              (converto 2 q2)
              (converto q3 :three)))))

(run-tests)
