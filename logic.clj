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

(run-tests)
