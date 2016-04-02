;;; queens.clj: Solves the N-Queens problem.
;;;
;;; Adapted from the code written by Mike Meyer.
;;; http://blog.mired.org/2011/03/easy-parallel-processing-in-clojure.html

(defn new-board
  "Create a new empty board of size n."
  [n]
  (vec (repeat n -1)))

(defn attack?
  "Returns true if it's possible from the first position [r1 c1] to atack
  the queen in the second position [r2 c2], otherwise returns false."
  [[r1 c1] [r2 c2]]
  (or
    (= r1 r2)
    (= c1 c2)
    (= (Math/abs (- r1 r2)) (Math/abs (- c1 c2)))))

(defn generate-next-boards
  "Returns a list with all the boards that can be produced starting from board
  and filling the corresponding col with every possible value."
  [board col]
  (for [i (range (count board))]
    (assoc board col i)))

(defn safe?
  "Returns true if all queens on board just before (but not including) col don't
  attack queen on col, otherwise returns false."
  [board col]
  (not (some #(attack? [(board col) col][(board %) %])
             (range col))))

(defn generate-next-safe-boards
  "Returns a list with all safe boards that can be produced starting from board
  and adding a new queen in col."
  [board col]
  (filter #(safe? % col) (generate-next-boards board col)))


(defn find-boards
  "Returns a list with all the solutions for the N-Queens problem starting with
  board already solved just before col. Stops when col is the last column in
  the board."
  [board col]
  (let [safe-boards (generate-next-safe-boards board col)]
    (if (= col (dec (count board)))
        safe-boards
        (reduce
          concat
            ((if (zero? col) pmap map) #(find-boards % (inc col)) safe-boards)))))
(defn queens
  "Returns a list with all the solutions for the N-Queens problem."
  [n]
  (find-boards (new-board n) 0))
