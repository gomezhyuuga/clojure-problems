; PROBLEM 8
; RESULT = 23514624000
; CODE:
(defn openfile
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq rdr))))

(defn number
  [n]
  (->>
    (openfile "08.txt")
    (reduce str)))

(defn product
  [s]
  (->>
    (map #(Character/digit % 10) s)
    (reduce *)))

(defn check
  [n]
  (->>
    (number n)
    (partition n 1)
    (sort #(compare (str %1) (str %2)))
    (map #(vector % (product %)))
    (sort #(compare (%1 1) (%2 1)))
    last))

(println (check 4))
(println (check 13))
