; PROBLEM 17
; RESULT =
; CODE:
(defn element-to-word
  "Returns the readable word representation of a number."
  [n]
  (condp = n
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    7 "seven"
    8 "eight"
    9 "nine"
    10 "ten"
    11 "eleven"
    12 "twelve"
    13 "thirteen"
    14 "fourteen"
    15 "fifteen"
    16 "sixteen"
    17 "seventeen"
    18 "eighteen"
    19 "nineteen"
    20 "twenty"
    30 "thirty"
    40 "forty"
    50 "fifty"
    60 "sixty"
    70 "seventy"
    80 "eighty"
    90 "ninety"))
(defn number-to-word
  [n]
  (let [thousands (quot n 1000)
        trem      (rem n 1000)
        hundreds  (quot trem 100)
        hrem      (rem n 100)
        dozens    (quot n 10)])
  [
   ])
