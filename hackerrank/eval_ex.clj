(doseq [line '("20.0000" "5.0000" "0.5000" "-0.5000")]
    (letfn [(pow [b e]
                (->>
                    (repeat e b)
                    (reduce *)))
            (fact [n]
                (if (< n 1)
                    1
                    (* n (fact (dec n)))))
            (aprox [n]
                (->>
                    (for [i (range 0 12)]
                        (/ (pow n i) (fact i)))
                    (reduce +)
                    double))]
        (println (aprox (read-string line)))))
