(use 'clojure.core.logic)

(defn my-last
  [lst]
  (cond
    (empty? (rest lst)) (first lst)
    (not (empty? (rest lst))) (my-last (rest lst))))

; (defn lasto
;   [lst result]
;   (fresh [h t]
;     (conde
;       [(conso result t lst) (== t [])]
;       [(conso h t lst) (!= t []) (lasto t result)])))

; With pattern matching
(defne lasto
  [lst result]
  ([[h] h])
  ([[h . t] result]
   (!= t [])
   (lasto t result)))

(defn my-reverse
  [lst]
  (cond
    (empty? lst)
    ()

    (not (empty? lst))
    (concat (my-reverse (rest lst)) [(first lst)])))

; (defn reverseo
;   [lst result]
;   (fresh [h t x]
;     (conde
;       [(== lst []) (== result [])]
;       [(!= lst [])
;        (conso h t lst)
;        (reverseo t x)
;        (appendo x [h] result)])))

; With pattern matching
(defne reverseo
  [lst result]
  ([[] []])
  ([[h . t] result]
   (fresh [x]
     (reverseo t x)
     (appendo x [h] result))))

(defn dup
  [lst]
  (cond
    (empty? lst)
    ()

    (not (empty? lst))
    (concat [(first lst) (first lst)]
            (dup (rest lst)))))

; (defn dupo
;   [lst result]
;   (conde
;     [(== lst [])
;     (== result [])]

;     [(!= lst [])
;     (fresh [h t x]
;       (conso h t lst)
;       (dupo t x)
;       (appendo [h h] x result))]))

; With pattern matching
(defne dupo
  [lst result]
  ([[] []])
  ([[h . t] result]
   (fresh [x]
     (dupo t x)
     (appendo [h h] x result))))
