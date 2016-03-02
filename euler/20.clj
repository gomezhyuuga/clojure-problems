(use 'clojure.test)

(defn factorial
  [n]
  (reduce * (take n (iterate inc 1N))))

(defn sum-digits
  [n]
  (->>
    (str n)
    (map #(bigint (Character/digit % 10)))
    (reduce +)))

(println "10! = " (factorial 10N))
(println "Sum of digits 10! = " (sum-digits (factorial 10N)))

(println "Sum of 100! = " (sum-digits (factorial 100N)))

; (run-tests)
