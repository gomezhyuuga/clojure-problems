(let [number (read-string (read-line))]
    (letfn [(fact [n]
                (cond
                    (<= n 1) 1
                    :else (* n (fact (dec n)))))
            (pascal [n]
                (->>
                    (range (inc n))
                    (map #(/ (fact n) (* (fact %) (fact (- n %)))))))]
    (->>
        (range number)
        (map #(clojure.string/join " " (pascal %)))
        (clojure.string/join "\n")
        println)))
