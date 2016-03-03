; PROBLEM 4
; RESULT = 906609
; CODE:
(defn palindrome?
  [s]
  (let [size (count s)
        half (quot size 2)]
    (if (= size 1)
      true
      (->>
        (for [i (range 0 half)
              :while (= (nth s i) (nth s (- (dec size) i)))] i)
        last
        (= (dec half))))))
(defn largest
  "Takes as argument a nonempty list of numbers lst. It returns the largest
  value contained in lst. Use the reduce function to solve this problem."
  [lst]
  (reduce (fn [a b] (if (> a b) a b)) lst))
(defn palindrome-product
  [a b]
  (->>
    (for [i (reverse (range a b)) j (range a b)] (* i j))
    (filter (fn [el] (palindrome? (str el))))
    largest))
(println "Largest palindrome of two digits = " (palindrome-product 10 100))
(println "Largest palindrome of three-digits = " (palindrome-product 100 1000))
