;;; ITESM CEM, April 14, 2016.
;;; Clojure Source File
;;; Activity: Macros
;;; Authors:
;;;          A01020319 Fernando Gómez Herrera

(ns macros)

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & y]
   `(let [expr# ~x]
      (if expr#
        expr#
        (my-or ~@y)))))

(defmacro do-loop
  [& args]
  (let [lastIns   (last args)
        op        (first lastIns)
        cnd       (second lastIns)
        eq        (if (= op :while) true false)
        exprs     (drop-last args)]
    `(loop []
       (do ~@exprs)
       (if (= ~eq ~cnd) (recur)))))

(defmacro defn-curry
  ([nam args & body]
  (let [x   (first args)
        els (rest args)]
    (cond
      (= 0 (count args)) `(defn ~nam []   (do ~@body))
      (= 1 (count args)) `(defn ~nam [~x] (do ~@body))
      :else              `(defn ~nam [~x] (curry ~body ~@els))))))
(defmacro curry
  ([body x]
   `(fn [~x] (do ~@body)))
  ([body x & more]
   `(fn [~x] (curry ~body ~@more))))

; TESTS
(use 'clojure.test)
(deftest test-my-or
  (is (= nil (my-or)))
  (is (= :one (my-or false :one nil :two false :three)))
  (is (= nil (my-or false false nil)))
  (is (= false (my-or nil nil false))))

(deftest test-do-loop
  (is (= "0\n1\n2\n3\n4\n"
         (with-out-str (do
                         (def i (atom 0))
                         (do-loop
                           (println @i)
                           (swap! i inc)
                           (:until (= @i 5)))))))
  (is (= "1\n2\n3\n4\n5\n"
         (with-out-str (do
                         (def j (atom 1))
                         (do-loop
                           (println @j)
                           (swap! j inc)
                           (:while (<= @j 5))))))))

(deftest test-defn-curry
  (do
    (defn-curry sum [a b c d] (prn 'args a b c d) (+ a b c d))
    (defn-curry go [x y] (* x (+ y 1)))
    (defn-curry add1 [x] (+ x 1))
    (defn-curry hello [] "hello")

    (is (= "args 1 2 3 4\n"
           (with-out-str
             ((((sum 1) 2) 3) 4))))
    (is (= 10 ((((sum 1) 2) 3) 4)))
    (is (= "args 15 8 16 42\n"
           (with-out-str
             ((((sum 15) 8) 16) 42))))
    (is (= 81 ((((sum 15) 8) 16) 42)))
    (is (= 8 ((go 2) 3)))
    (is (= 9 ((go 3) 2)))
    (is (= 1 (add1 0)))
    (is (= 42 (add1 41)))
    (is (= "hello" (hello)))))
(run-tests)

