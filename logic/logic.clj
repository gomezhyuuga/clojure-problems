(use 'clojure.core.logic)

(defn my-last
  [lst]
  (cond
    (empty? (rest lst)) (first lst)
    (not (empty? (rest lst))) (my-last (rest lst))))

(defn lasto "logic last" [lst result]
  (fresh [h t]
         (conde
           [(conso result t lst) (== t [])]
           [(conso h t lst)      (!= t []) (lasto t result)])))

(defn reverseo "reverse a list" [lst result]
  (fresh [h t x]
         (conde
           [(== lst []) (== result [])]
           [(!= lst [])
            (conso h t lst)
            (reverseo t x)
            (appendo x [h] result)])))

(defne lasto
  [lst result]
  ([[h] h])
  ([[h . t] result]
   (!= t [])
   (lasto t result)))
