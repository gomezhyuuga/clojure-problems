;==========================================================
; Type your name and student ID here.
;==========================================================

(use 'clojure.test)

;==========================================================
(defn area
  [d]
  (/ (* 3.1416 (* d d)) 4))
(defn cost-per-inch
  [[d p]]
  [d (/ (area d) p)])
(defn pizza
  "Takes a list of vectors lst. Each vector contains two
  integers respectively designating a pizza's diameter d
  (in inches) and price p (in dollars). It returns the
  diameter d of the pizza with the best value."
  [lst]
  (->>
    (map cost-per-inch lst)
    (sort-by (fn [x] (x 1)))
    last
    first))

;==========================================================
(defrecord State [state-name abbreviation])

(def s1 (State. "California" :CA))
(def s2 (State. "Oregon" :OR))
(def s3 (State. "Texas" :TX))

(defrecord Item [price])
(defrecord Gasoline [price-per-gallon gallons])
(defrecord Cigarettes [price])

(def i (Item. 9.99))
(def g (Gasoline. 3.75 15))
(def c (Cigarettes. 4.50))

(defmulti total
  "Multimethod that allows you to compute the total price,
  including tax, of a certain article in a certain state."
  (fn [article state]
    [ (class article) (:abbreviation state) ]))

(defmethod total :default
  [article state]
  nil)

(defmethod total [Cigarettes :CA] [article state] (+ (:price article) 0.87))
(defmethod total [Cigarettes :OR] [article state] (+ (:price article) 1.18))
(defmethod total [Cigarettes :TX] [article state] (+ (:price article) 1.41))

(defmethod total [Gasoline :CA] [article state]
  (* (:gallons article) (+ (:price-per-gallon article) 0.46)))
(defmethod total [Gasoline :OR] [article state]
  (* (:gallons article) (+ (:price-per-gallon article) 0.25)))
(defmethod total [Gasoline :TX] [article state]
  (* (:gallons article) (+ (:price-per-gallon article) 0.20)))

(defmethod total [Item :CA] [article state] (* (:price article) 1.0825))
(defmethod total [Item :OR] [article state] (:price article))
(defmethod total [Item :TX] [article state] (* (:price article) 1.0625))

;==========================================================
(deftest test-pizza
  (is (= 12 (pizza '([5 2] [10 6] [12 8]))))
  (is (= 10 (pizza '([5 2] [10 5] [12 8]))))
  (is (= 24 (pizza '([1 1] [24 33] [13 11] [6 11])))))

;==========================================================

(deftest test-total
  (is (= 10.814175 (total i s1)))
  (is (= 9.99 (total i s2)))
  (is (= 10.614375 (total i s3)))
  (is (= 63.15 (total g s1)))
  (is (= 60.0 (total g s2)))
  (is (= 59.25 (total g s3)))
  (is (= 5.37 (total c s1)))
  (is (= 5.68 (total c s2)))
  (is (= 5.91 (total c s3))))

;==========================================================
(run-tests)

