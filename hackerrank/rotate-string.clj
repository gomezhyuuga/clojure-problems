(use 'clojure.test)


(defn rotations
  [s]
  (let [n (inc (count s))]
    (->>
      (range 1 n)
      (map #(take (dec n) (drop % (cycle s))))
      (map #(reduce str %)))))

(deftest test-rotations
  (is (= '("bca" "cab" "abc") (rotations "abc")))
  (is (= '("bcdea" "cdeab" "deabc" "eabcd" "abcde") (rotations "abcde")))
  (is (= '("baba" "abab" "baba" "abab") (rotations "abab")))
  (is (= '("aaa" "aaa" "aaa") (rotations "aaa"))))

(run-tests)
