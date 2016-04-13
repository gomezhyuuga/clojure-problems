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
(run-tests)
