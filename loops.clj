(defn pow
  "Raises b to the power e."
  [b e]
  (if (zero? e)
    1N
    (* b (pow b (dec e)))))

(defn dup
  "Returns a list with all the elements of x duplicated."
  [x]
  (if (empty? x)
    ()
    (cons (first x)
          (cons (first x)
                (dup (rest x))))))
