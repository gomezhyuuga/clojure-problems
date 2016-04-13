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

(use 'clojure.test)
(deftest test-my-or
  (is (= nil (my-or)))
  (is (= :one (my-or false :one nil :two false :three)))
  (is (= nil (my-or false false nil)))
  (is (= false (my-or nil nil false))))
(run-tests)
