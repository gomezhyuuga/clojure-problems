#!/usr/bin/env lein exec
(defn replace-keyword
  "Replaces keyword with an expression to get the value of that column."
  [expression freplacer]
  (loop [res () coll expression]
    (let [f (first coll)
          r (rest coll)]
    (cond
      (empty? coll) (reverse res)
      (list? f)     (recur (cons (replace-keyword f freplacer) res) r)
      (keyword? f)  (recur (cons (freplacer f) res) r)
      :else         (recur (cons f res) r)))))

(defmacro meval
  [lst]
  `(~@lst))

;(def age 18)
;(def expr '(and (= :age 18) (= :age age)))
;(def expr1 '(= :age 18))
;(def row '(417 "Tony Stark" 35))
;(def expr '(= 18 12))
;(println "EXPR: " expr)
;(println "MACRO EXPAND")
;(println (clojure.walk/macroexpand-all '(my-eval (= 18 12))))
;(println "EVAL")
;(println (my-eval (= 12 11)))
;;(replace-keyword [] #(str %))
;;(replace-keyword [:one] identity)
