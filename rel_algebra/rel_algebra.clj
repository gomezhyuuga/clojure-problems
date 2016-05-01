;----------------------------------------------------------
; Activity: Project: Relational Algebra DSL
; Date: May 4, 2016.
; Author:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------
(use '[clojure.string :only (join split-lines)]
     '[clojure.test])

(declare difference
         intersection
         product
         project
         relation
         rename
         select
         union)
; UTILS
(declare widest str-relation)

(defrecord Relation
  [column-names rows]
  Object
  (toString [this]
    (str-relation this)))
(defmethod print-method Relation
  [this w]
  (print-simple (str this) w))
(defn check-argument
  "Check if condition is true. If not, throw an
  IllegalArgumentException with the given error-message."
  [condition error-message]
  (when (not condition)
    (throw (IllegalArgumentException. error-message))))

(defn replace-keyword
  "Replaces keyword with an expression to get the value of that column."
  [expression freplacer]
  (loop [res () coll expression]
    (let [f (first coll)
          r (rest coll)]
    (cond
      (empty? coll) (reverse res)
      (list? f)     (recur (cons (replace-keyword f freplacer) res) r)
      (keyword? f)  (recur (cons (freplacer f) res) r)
      :else         (recur (cons f res) r)))))
(defn get-column
  "Get values for the specified column"
  [index data]
  (map #(nth % index) data))
(defn column-indices
  [headers]
    (->>
      (map-indexed #(vector %2 %1) headers)
      flatten
      (apply hash-map)))
(defn column-value
  [row column headers]
  (let [indices (column-indices headers)
        value   (nth row (indices (name column)))]
    value))
(defn check-record
  [expression record headers]
  (let [condition (replace-keyword expression #(column-value record % headers))]
    (eval condition)))
(defn find
  [el lst]
  "Finds an item inside a list. Returns the record if it exists, nil otherwise."
  (->>
    (drop-while #(not= el %) lst)
    first))
(defn find-index
  [el lst]
  "Finds an item inside a list. Returns the index of the item"
  (->>
    (map-indexed #(vector %1 %2) lst)
    (drop-while (fn [[_ v]] (not= v el)))
    first
    first))
(defn nths
  [lst indexes]
  (map #(nth lst %) indexes))

(defn wrap-with
  "Wraps a string in + sign"
  [wrapper data]
  (str wrapper data wrapper))
(defn build-border
  "Border for a formatted table"
  [colSizes]
  (->>
    (map #(repeat (+ 2 %) "-") colSizes)
    (map join)
    (join  "+")
    (wrap-with "+")))
(defn format-value
  "Formats integer values right justified and other values left justified"
  [value size]
  (if (integer? value)
    (format (str " %"  size "d ") value)
    (format (str " %-" size "s ") value)))
(defn build-row
  "Row formatted for a table"
  [colSizes data]
  (->>
    (map-indexed #(format-value %2 (nth colSizes %1)) data)
    (join "|")
    (wrap-with "|")))
(defn build-header
  "Creates a table header"
  [colSizes headers]
  (build-row colSizes headers))
(defn build-body
  "Creates the body fo the table"
  [colSizes rows]
  (->>
    (map #(build-row colSizes %) rows)
    (join "\n")))

(defn str-relation
  "Creates a string representation of a Relation, i.e. a table"
  [record]
  (let [headers   (.column-names record) ; Column names
        rows      (.rows record) ; Records for the table
        nCols     (count headers) ; Number of columns
        all       (cons headers rows) ; All the rows (inc headers) in a single list
        colValues (map #(get-column % all) (range nCols)) ; List of column values
        colSizes  (map widest colValues) ; List with the sizes of each column
        border    (build-border colSizes) ; Horizontal border of the table
        fHeader   (build-header colSizes headers) ; Formatted header
        fBody     (build-body colSizes rows)]
    (join "\n" [border fHeader border fBody border])))

(defn create-record
  "Creates a relation record with attributes [column-names rows]"
  [lst]
  (Relation. (first lst) (rest lst)))
(defn convert
  "Read a list of string values and if converts number to their respective
  integer type."
  [lst]
  (map #(if (re-matches #"\d+" %) (read-string %) %) lst))
(defn split
  "Split by commas"
  [s]
  (clojure.string/split s #","))
(defn read-csv
  "Reads a CSV file and splits into lines"
  [filename]
  (let [data   (split-lines (slurp filename))
        header (first data)
        rows   (rest data)]
    (->>
      (map split rows)
      (map convert)
      (cons (split header)))))
(defn widest
  "Get the length of the widest elemenet as str in the list"
  [lst]
  (->>
    (map #(count (str %)) lst)
    (reduce max)))
(defn product-record
  "Product between a single record and a relation"
  [record relation]
  (map #(concat record %) (.rows relation)))
(defn check-union-compatibility
  "Checks if two relations are union-compatible"
  [relation-a relation-b]
  (check-argument
    (= (.column-names relation-a) (.column-names relation-b))
    (str "Both relations must be union-compatible (have the exact same
         attributes and in the same order)")))
(defn check-every-is-keyword
  "Validates that all the elements are keywords"
  [param]
  (check-argument
    (every? keyword? param)
    (str "Every element in attribute-vector must be a keyword")))
(defn check-for-vector
  "Validates that the parameter is a vector"
  [param]
  (check-argument
    (vector? param)
    (str "The parameter must be a vector, not a" (class param))))
(defn check-uniqueness
  "Validates that all the elements in parameter are different"
  [param]
  (check-argument
    (= (count param) (count (distinct param)))
    (str "All elements in attribute-vector must be different")))
(defn check-valid-keywords
  "Validates that all keywords belongs to attributes in relation"
  [keywords relation]
  (check-argument
    (every? #(find % (.column-names relation)) (map name keywords))
    (str "All keywords in expression must refer to an attribute in relation")))

(defn relation
  "This factory function creates a new relation object, taking the data from a
  table contained in a CSV file. The relation object must be an instance of a
  record (created with defrecord) or type (created with deftype). The parameter
  file-name must be a keyword naming a file with a .csv extension contained in
  the current directory."
  [file-name]

  (check-argument
    (keyword? file-name)
    (str "Parameter" file-name "must be a keyword, not a" (class file-name)))

  (->>
    (read-csv (str (name file-name) ".csv"))
    create-record))

(defn union
  "Returns a new relation object that contains all the rows in relation-a and relation-b."
  [relation-a relation-b]
  (check-union-compatibility relation-a relation-b)

  (let [rows-a (.rows relation-a)
        rows-b (.rows relation-b)
        header (.column-names relation-a)
        rows   (distinct (concat rows-a rows-b))]
    (Relation. header rows)))

(defn difference
  "Returns a new relation object that contains the rows in relation-a that are
  not in relation-b."
  [relation-a relation-b]
  (check-union-compatibility relation-a relation-b)

  (let [rows-a (.rows relation-a)
        rows-b (.rows relation-b)
        header (.column-names relation-a)
        rows   (filter #(not (find % rows-b)) rows-a)]
    (Relation. header rows)))

(defn intersection
  "Returns a new relation object that contains the rows in relation-a that are
  also in relation-b."
  [relation-a relation-b]
  (check-union-compatibility relation-a relation-b)

  (let [rows-a (.rows relation-a)
        rows-b (.rows relation-b)
        header (.column-names relation-a)
        rows   (filter #(find % rows-b) rows-a)]
    (Relation. header rows)))

(defn product
  "Returns a new relation object that contains the Cartesian product of
  relation-a times relation-b."
  [relation-a relation-b]
  (check-argument
    (every? #(not (find % (.column-names relation-a))) (.column-names relation-b))
    (str "There can't be common attributes in both relations"))

  (let [header (concat (.column-names relation-a) (.column-names relation-b))
        rows   (reduce concat (map #(product-record % relation-b) (.rows relation-a)))]
    (Relation. header rows)))

(defn project
  "Returns a new relation object based on relation but only with the columns
  specified in attribute-vector."
  [attribute-vector relation]
  (check-for-vector attribute-vector)
  (check-argument
    (not (empty? attribute-vector))
    (str "First parameter attribute-vector can't be empty"))
  (check-every-is-keyword attribute-vector)
  (check-uniqueness attribute-vector)
  (check-argument
    (every? #(find % (.column-names relation)) (map name attribute-vector))
    (str "attribute-vector can't contain attributes that does not exist in the relation"))

  (let [header  (.column-names relation)
        columns (map name attribute-vector)
        colNums (map #(find-index % header) columns)
        rows    (distinct (map #(nths % colNums) (.rows relation)))]
    (Relation. columns rows)))

(defn rename
  "Returns a new relation object which has the same rows as relation but with all its columns
  renamed using the names contained in attribute-vector.
  The attribute names must be unique keywords in attribute-vector. The number of attributes in
  attribute-vector must be the same as the number of columns in relation."
  [attribute-vector relation]
  (check-every-is-keyword attribute-vector)
  (check-for-vector attribute-vector)
  (check-uniqueness attribute-vector)
  (check-argument
    (= (count attribute-vector) (count (.column-names relation)))
    (str "There number of attributes must be the same as the relation's"))

  (Relation. (map name attribute-vector) (.rows relation)))

(defn get-keywords
  "Returns a list with all the keywords inside a list"
  [lst]
  (->>
    (flatten '~expression)
    (filter keyword?)
    (map name)))

(defmacro select
  "This operation has to be implemented as a macro. It returns a new relation
  object containing all the rows in relation that meet the condition established
  in expression, which can be any Clojure expression. Any keyword used as part
  of expression must refer to an attribute in relation."
  [expression relation]
  `(let [headers#  (.column-names ~relation)
         rows#     (.rows ~relation)
         keywords# (get-keywords '~expression)
         valid#    (check-valid-keywords keywords# ~relation)
         data#     (filter #(eval (check-record '~expression % headers#)) rows#)]
    (Relation. headers# data#)))

(deftest test-convert
  (is
    (= '(1 3 "ok" "ok2" "2ok" "string" 2345)
       (convert '("1" "3" "ok" "ok2" "2ok" "string" "2345")))))
(deftest test-widest
  (is (= 5 (widest '("a" "ab" "abc" "abcd" "abcde")))) ; 5 = abcde
  (is (= 3 (widest '("id" 1 2 12 20 300 22 111)))) ; 3 = 300 or 111
  (is (= 16 (widest '("Gwen Stacy" "Natalia Romanova" "Tony Stark" "Peggy Carter"
                            "Peter Parker" "Pepper Potts")))))
(deftest test-find
  (do
    (def a '(199 "Gwen Stacy" 18))
    (def b '(598 "MaryJane Watson" 18))
    (def s1 (relation :students1))
    (def s2 (relation :students2))
    (is (= a (find a (.rows s1))))
    (is (= nil (find a (.rows s2))))
    (is (= nil (find b (.rows s1))))))
(deftest test-column-indices
  (do
    (def s1 (relation :students1))
    (def p (relation :pizzas))
    (is (= {"id" 0, "name" 1, "age" 2} (column-indices (.column-names s1))))
    (is (= {"size" 1, "flavor" 0} (column-indices (.column-names p))))))

(def s1 (relation :students1))
(def s2 (relation :students2))
(def c (relation :courses))
(def e (relation :enrollments))
(def p (relation :pizzas))

(deftest test-relation
  (is (=
        (str "+-----+------------------+-----+\n"
             "| id  | name             | age |\n"
             "+-----+------------------+-----+\n"
             "| 199 | Gwen Stacy       |  18 |\n"
             "| 286 | Natalia Romanova |  25 |\n"
             "| 417 | Tony Stark       |  45 |\n"
             "| 430 | Peggy Carter     |  91 |\n"
             "| 447 | Peter Parker     |  18 |\n"
             "| 505 | Pepper Potts     |  32 |\n"
             "+-----+------------------+-----+")
        (str s1)))
  (is (=
        (str "+-----+------------------+------+\n"
             "| id  | name             | age  |\n"
             "+-----+------------------+------+\n"
             "| 286 | Natalia Romanova |   25 |\n"
             "| 430 | Peggy Carter     |   91 |\n"
             "| 505 | Pepper Potts     |   32 |\n"
             "| 528 | Steve Rogers     |   93 |\n"
             "| 559 | Jane Foster      |   29 |\n"
             "| 598 | MaryJane Watson  |   18 |\n"
             "| 666 | Damien Thorn     |   66 |\n"
             "| 824 | Bruce Banner     |   42 |\n"
             "| 993 | Diana Prince     | 3217 |\n"
             "+-----+------------------+------+")
        (str s2)))
  (is (=
        (str "+--------+----------------------------------+--------+------+\n"
             "| code   | name                             | time   | room |\n"
             "+--------+----------------------------------+--------+------+\n"
             "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
             "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
             "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
             "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
             "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
             "+--------+----------------------------------+--------+------+")
        (str c)))
  (is (=
        (str "+--------+-----+\n"
             "| code   | id  |\n"
             "+--------+-----+\n"
             "| TC2025 | 824 |\n"
             "| TC2025 | 286 |\n"
             "| TC2025 | 528 |\n"
             "| TC2006 | 528 |\n"
             "| TC2006 | 505 |\n"
             "| TC2006 | 993 |\n"
             "| TC2026 | 505 |\n"
             "| TC2026 | 666 |\n"
             "| TC2026 | 598 |\n"
             "| TC3049 | 430 |\n"
             "| TC3049 | 447 |\n"
             "| TC3049 | 666 |\n"
             "| TC3048 | 666 |\n"
             "| TC3048 | 417 |\n"
             "| TC3048 | 430 |\n"
             "+--------+-----+")
        (str e)))
  (is (= (str "+-----------+--------+\n"
              "| flavor    | size   |\n"
              "+-----------+--------+\n"
              "| Hawaiian  | Medium |\n"
              "| Pepperoni | Large  |\n"
              "| Supreme   | Small  |\n"
              "+-----------+--------+")
         (str p)))
  (is (thrown?
        IllegalArgumentException
        (relation 42))
      "Should throw exception if parameter to relation is not a keyword.")
  (is (thrown?
        java.io.FileNotFoundException
        (relation :inexistent))
      "Should throw exception if the requested file was not found."))

(deftest test-union
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "+-----+------------------+------+")
         (str (union s1 s2))))
  (is (= (str "+-----+------------------+------+\n"
              "| id  | name             | age  |\n"
              "+-----+------------------+------+\n"
              "| 286 | Natalia Romanova |   25 |\n"
              "| 430 | Peggy Carter     |   91 |\n"
              "| 505 | Pepper Potts     |   32 |\n"
              "| 528 | Steve Rogers     |   93 |\n"
              "| 559 | Jane Foster      |   29 |\n"
              "| 598 | MaryJane Watson  |   18 |\n"
              "| 666 | Damien Thorn     |   66 |\n"
              "| 824 | Bruce Banner     |   42 |\n"
              "| 993 | Diana Prince     | 3217 |\n"
              "| 199 | Gwen Stacy       |   18 |\n"
              "| 417 | Tony Stark       |   45 |\n"
              "| 447 | Peter Parker     |   18 |\n"
              "+-----+------------------+------+")
         (str (union s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (union c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (union c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-difference
  (is (= (str  "+-----+--------------+-----+\n"
               "| id  | name         | age |\n"
               "+-----+--------------+-----+\n"
               "| 199 | Gwen Stacy   |  18 |\n"
               "| 417 | Tony Stark   |  45 |\n"
               "| 447 | Peter Parker |  18 |\n"
               "+-----+--------------+-----+")
         (str (difference s1 s2))))
  (is (= (str "+-----+-----------------+------+\n"
              "| id  | name            | age  |\n"
              "+-----+-----------------+------+\n"
              "| 528 | Steve Rogers    |   93 |\n"
              "| 559 | Jane Foster     |   29 |\n"
              "| 598 | MaryJane Watson |   18 |\n"
              "| 666 | Damien Thorn    |   66 |\n"
              "| 824 | Bruce Banner    |   42 |\n"
              "| 993 | Diana Prince    | 3217 |\n"
              "+-----+-----------------+------+")
         (str (difference s2 s1))))
  (is (thrown?
        IllegalArgumentException
        (difference c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (difference c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-intersection
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (intersection s1 s2))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| code   | name                             | time   | room |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (intersection c c))))
  (is (thrown?
        IllegalArgumentException
        (intersection c e))
      "Should throw exception if attributes in both relations are not union-compatible (have the same name and same order).")
  (is (thrown?
        IllegalArgumentException
        (intersection c 42))
      "Should throw exception if a parameter is a not a relation."))

(deftest test-product
  (is (= (str "+-----+------------------+-----+-----------+--------+\n"
              "| id  | name             | age | flavor    | size   |\n"
              "+-----+------------------+-----+-----------+--------+\n"
              "| 199 | Gwen Stacy       |  18 | Hawaiian  | Medium |\n"
              "| 199 | Gwen Stacy       |  18 | Pepperoni | Large  |\n"
              "| 199 | Gwen Stacy       |  18 | Supreme   | Small  |\n"
              "| 286 | Natalia Romanova |  25 | Hawaiian  | Medium |\n"
              "| 286 | Natalia Romanova |  25 | Pepperoni | Large  |\n"
              "| 286 | Natalia Romanova |  25 | Supreme   | Small  |\n"
              "| 417 | Tony Stark       |  45 | Hawaiian  | Medium |\n"
              "| 417 | Tony Stark       |  45 | Pepperoni | Large  |\n"
              "| 417 | Tony Stark       |  45 | Supreme   | Small  |\n"
              "| 430 | Peggy Carter     |  91 | Hawaiian  | Medium |\n"
              "| 430 | Peggy Carter     |  91 | Pepperoni | Large  |\n"
              "| 430 | Peggy Carter     |  91 | Supreme   | Small  |\n"
              "| 447 | Peter Parker     |  18 | Hawaiian  | Medium |\n"
              "| 447 | Peter Parker     |  18 | Pepperoni | Large  |\n"
              "| 447 | Peter Parker     |  18 | Supreme   | Small  |\n"
              "| 505 | Pepper Potts     |  32 | Hawaiian  | Medium |\n"
              "| 505 | Pepper Potts     |  32 | Pepperoni | Large  |\n"
              "| 505 | Pepper Potts     |  32 | Supreme   | Small  |\n"
              "+-----+------------------+-----+-----------+--------+")
         (str (product s1 p))))
  (is (= (str "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| flavor    | size   | code   | name                             | time   | room |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+\n"
              "| Hawaiian  | Medium | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Hawaiian  | Medium | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Hawaiian  | Medium | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Hawaiian  | Medium | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Hawaiian  | Medium | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Pepperoni | Large  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Pepperoni | Large  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Pepperoni | Large  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Pepperoni | Large  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Pepperoni | Large  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "| Supreme   | Small  | TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| Supreme   | Small  | TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| Supreme   | Small  | TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| Supreme   | Small  | TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| Supreme   | Small  | TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+-----------+--------+--------+----------------------------------+--------+------+")
         (str (product p c))))
  (is (thrown?
        IllegalArgumentException
        (product c 42))
      "Should throw exception if a parameter is a not a relation.")
  (is (thrown?
        IllegalArgumentException
        (product c s1))
      "Should throw exception if attributes in both relations are not distinct."))

(deftest test-project
  (is (= (str "+-----+\n"
              "| age |\n"
              "+-----+\n"
              "|  18 |\n"
              "|  25 |\n"
              "|  45 |\n"
              "|  91 |\n"
              "|  32 |\n"
              "+-----+")
         (str (project [:age] s1))))
  (is (= (str "+------+----------------------------------+\n"
              "| room | name                             |\n"
              "+------+----------------------------------+\n"
              "| 5204 | Advanced Programming             |\n"
              "| 6307 | Programming Languages            |\n"
              "| 4207 | Web Applications Development     |\n"
              "| 4310 | Software Design and Architecture |\n"
              "| 4202 | Compiler Design                  |\n"
              "+------+----------------------------------+")
         (str (project [:room :name] c))))
  (is (= (str "+--------+\n"
              "| code   |\n"
              "+--------+\n"
              "| TC2025 |\n"
              "| TC2006 |\n"
              "| TC2026 |\n"
              "| TC3049 |\n"
              "| TC3048 |\n"
              "+--------+")
         (str (project [:code] e))))
  (is (= (str "+-----+------------------+-----+\n"
              "| id  | name             | age |\n"
              "+-----+------------------+-----+\n"
              "| 199 | Gwen Stacy       |  18 |\n"
              "| 286 | Natalia Romanova |  25 |\n"
              "| 417 | Tony Stark       |  45 |\n"
              "| 430 | Peggy Carter     |  91 |\n"
              "| 447 | Peter Parker     |  18 |\n"
              "| 505 | Pepper Potts     |  32 |\n"
              "+-----+------------------+-----+")
         (str (project [:id :name :age] s1))))
  (is (thrown?
        IllegalArgumentException
        (project '(:code) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (project [] c))
      "Should throw exception if attribute vector is empty.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code 'time :room] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code :name :time] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :inexistent :code] c))
      "Should throw exception if the attribute vector includes an attribute that does not exist in the relation.")
  (is (thrown?
        IllegalArgumentException
        (project [:name :code] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-rename
  (is (= (str "+------------+------------------+-------------+\n"
              "| student-id | student-name     | student-age |\n"
              "+------------+------------------+-------------+\n"
              "|        199 | Gwen Stacy       |          18 |\n"
              "|        286 | Natalia Romanova |          25 |\n"
              "|        417 | Tony Stark       |          45 |\n"
              "|        430 | Peggy Carter     |          91 |\n"
              "|        447 | Peter Parker     |          18 |\n"
              "|        505 | Pepper Potts     |          32 |\n"
              "+------------+------------------+-------------+")
         (str (rename [:student-id :student-name :student-age] s1))))
  (is (= (str "+--------+----------------------------------+--------+------+\n"
              "| a      | b                                | c      | d    |\n"
              "+--------+----------------------------------+--------+------+\n"
              "| TC2025 | Advanced Programming             | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages            | We1900 | 6307 |\n"
              "| TC2026 | Web Applications Development     | Tu1900 | 4207 |\n"
              "| TC3049 | Software Design and Architecture | Th1600 | 4310 |\n"
              "| TC3048 | Compiler Design                  | Fr1900 | 4202 |\n"
              "+--------+----------------------------------+--------+------+")
         (str (rename [:a :b :c :d] c))))
  (is (thrown?
        IllegalArgumentException
        (rename '(:c :n :t :r) c))
      "Should throw exception if first parameter is not a vector.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n 't :r] c))
      "Should throw exception if in the attribute vector there's a non-keyword.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :c :r] c))
      "Should throw exception if the attribute vector has repeated values.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :r] c))
      "Should throw exception if the number of attributes in the attribute vector and in the relation are not the same.")
  (is (thrown?
        IllegalArgumentException
        (rename [:c :n :t :r] 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-select
  (is (= (str "+-----+------------+-----+\n"
              "| id  | name       | age |\n"
              "+-----+------------+-----+\n"
              "| 417 | Tony Stark |  45 |\n"
              "+-----+------------+-----+")
         (str (select (= :id 417) s1))))
  (is (= (str "+-----+--------------+-----+\n"
              "| id  | name         | age |\n"
              "+-----+--------------+-----+\n"
              "| 199 | Gwen Stacy   |  18 |\n"
              "| 447 | Peter Parker |  18 |\n"
              "| 505 | Pepper Potts |  32 |\n"
              "+-----+--------------+-----+")
         (str (select (or (< :age 20) (> :id 500)) s1))))
  (is (= (str "+--------+-----------------------+--------+------+\n"
              "| code   | name                  | time   | room |\n"
              "+--------+-----------------------+--------+------+\n"
              "| TC2025 | Advanced Programming  | Mo1600 | 5204 |\n"
              "| TC2006 | Programming Languages | We1900 | 6307 |\n"
              "+--------+-----------------------+--------+------+")
         (str (select
                (or (= :code "TC2025")
                    (and (zero? (rem :room 7))
                         (= "We" (subs :time 0 2))))
                c))))
  (is (thrown?
        IllegalArgumentException
        (select (= :inexistent 42)  s1) )
      "Should throw exception if using in select expression an attribute that does not exist in relation.")
  (is (thrown?
        IllegalArgumentException
        (select (= :age 18) 42))
      "Should throw exception if the second parameter is not a relation."))

(deftest test-composed-1
  "In what room and at what time is the 'Web Applications Development' course taught?"
  (is (= (str "+------+--------+\n"
              "| room | time   |\n"
              "+------+--------+\n"
              "| 4207 | Tu1900 |\n"
              "+------+--------+")
         (str (->>
                (select (= :name "Web Applications Development") c)
                (project [:room :time]))))))

(deftest test-composed-2
  "List the name of all the students who are between 45 and 100 years old."
  (is (= (str "+--------------+\n"
              "| name         |\n"
              "+--------------+\n"
              "| Tony Stark   |\n"
              "| Peggy Carter |\n"
              "| Steve Rogers |\n"
              "| Damien Thorn |\n"
              "+--------------+")
         (str (->>
                (union s1 s2)
                (select (<= 45 :age 100))
                (project [:name]))))))

(deftest test-composed-3
  "For two pizzas, list all the possible flavor permutations."
  (is (= (str "+-----------+-----------+\n"
              "| pizza-1   | pizza-2   |\n"
              "+-----------+-----------+\n"
              "| Hawaiian  | Hawaiian  |\n"
              "| Hawaiian  | Pepperoni |\n"
              "| Hawaiian  | Supreme   |\n"
              "| Pepperoni | Hawaiian  |\n"
              "| Pepperoni | Pepperoni |\n"
              "| Pepperoni | Supreme   |\n"
              "| Supreme   | Hawaiian  |\n"
              "| Supreme   | Pepperoni |\n"
              "| Supreme   | Supreme   |\n"
              "+-----------+-----------+")
         (str (->>
                (project [:flavor] p)
                (rename [:f])
                (product p)
                (project [:flavor :f])
                (rename [:pizza-1 :pizza-2]))))))

(deftest test-composed-4
  "List the name of all the courses in which 'Damien Thorn' is enrolled."
  (is (= (str "+----------------------------------+\n"
              "| name                             |\n"
              "+----------------------------------+\n"
              "| Web Applications Development     |\n"
              "| Software Design and Architecture |\n"
              "| Compiler Design                  |\n"
              "+----------------------------------+")
         (str (->>
                (union s1 s2)
                (select (= :name "Damien Thorn"))
                (project [:id])
                (rename [:s-id])
                (product e)
                (select (= :id :s-id))
                (project [:code])
                (rename [:e-code])
                (product c)
                (select (= :code :e-code))
                (project [:name]))))))

(deftest test-composed-5
  "List the name of all the students who are not enrolled in the 'Programming Languages' course."
  (is (= (str "+------------------+\n"
              "| name             |\n"
              "+------------------+\n"
              "| Gwen Stacy       |\n"
              "| Natalia Romanova |\n"
              "| Tony Stark       |\n"
              "| Peggy Carter     |\n"
              "| Peter Parker     |\n"
              "| Jane Foster      |\n"
              "| MaryJane Watson  |\n"
              "| Damien Thorn     |\n"
              "| Bruce Banner     |\n"
              "+------------------+")
         (str (let [s (union s1 s2)]
                (->>
                  (select (= :name "Programming Languages") c)
                  (project [:code])
                  (rename [:c-code])
                  (product e)
                  (select (= :code :c-code))
                  (project [:id])
                  (rename [:e-id])
                  (product s)
                  (select (= :id :e-id))
                  (project [:id :name :age])
                  (difference s)
                  (project [:name])))))))

(deftest test-composed-6
  "List the id and name of all the students that are enrolled in both the 'Compiler Design' and 'Software Design and Architecture' courses."
  (is (= (str "+-----+--------------+\n"
              "| id  | name         |\n"
              "+-----+--------------+\n"
              "| 430 | Peggy Carter |\n"
              "| 666 | Damien Thorn |\n"
              "+-----+--------------+")
         (str (let [s (union s1 s2)
                    students-by-course
                     (fn [course-name]
                       (->>
                         (select (= :name course-name) c)
                         (project [:code])
                         (rename [:c-code])
                         (product e)
                         (select (= :code :c-code))
                         (project [:id])
                         (rename [:e-id])
                         (product s)
                         (select (= :id :e-id))
                         (project [:id :name])))]
                (->>
                  (intersection
                    (students-by-course "Compiler Design")
                    (students-by-course "Software Design and Architecture"))))))))
(run-tests)
