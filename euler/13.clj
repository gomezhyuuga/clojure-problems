; PROBLEM 13
; RESULT = 5537376230
; CODE:
(defn openfile
  []
  (with-open [rdr (clojure.java.io/reader "bignum.txt")]
    (reduce conj [] (line-seq rdr))))

(defn sum
  []
  (->>
    (openfile)
    (map bigint)
    (reduce +)
    str
    (take 10)
    (reduce str)))
(println (sum))
