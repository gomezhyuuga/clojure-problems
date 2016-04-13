(defmacro debug
  [fun]
  `(let [temp# ~fun]
      (printf "debug: %s => %s%n" '~fun temp#)
      temp#))

(defn fact
  [n]
  (if (zero? n)
      1
      (debug (* n (fact (dec n))))))

(println (fact 5))
