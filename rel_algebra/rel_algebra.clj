#!/usr/bin/env lein exec
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

(defmacro replace-keyword
  "Replaces keyword with an expression to get the value of that column."
  [expression freplacer]
  (loop [res () coll expression]
    (let [f (first coll)
          r (rest coll)]
    (cond
      (empty? coll) (reverse res)
      (list? f)     (recur (cons `(replace-keyword ~f ~freplacer) res) r)
      (keyword? f)  (recur (cons `(~freplacer ~f) res) r)
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
;(defn check-record
  ;[expression record headers]
  ;(let [condition (replace-keyword expression #(column-value record % headers))]
    ;(println expression)
    ;(println condition)))

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
    (every? #(find % (.column-names relation)) keywords)
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
    (flatten lst)
    (filter keyword?)
    (map name)))

(defmacro select
  "This operation has to be implemented as a macro. It returns a new relation
  object containing all the rows in relation that meet the condition established
  in expression, which can be any Clojure expression. Any keyword used as part
  of expression must refer to an attribute in relation."
  [expression relation]
  `(let [headers# (.column-names ~relation)
         rows# (.rows ~relation)
         row# (nth rows# 0)
         data# (filter (fn [rr#]
                   (replace-keyword ~expression #(column-value rr# % headers#))) rows#)]
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


