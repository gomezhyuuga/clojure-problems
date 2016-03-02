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
    (take 10)))
(println (sum))
