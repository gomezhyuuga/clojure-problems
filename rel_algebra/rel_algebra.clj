;----------------------------------------------------------
; Activity: Project: Relational Algebra DSL
; Date: May 4, 2016.
; Author:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(ns gh.rel-algebra
  (:require [clojure.string :as str])
  (:use clojure.test))

(declare difference
         intersection
         product
         project
         relation
         rename
         select
         union)
; UTILS
(declare widest)
(defrecord Relation
  [column-names rows])

;; Override Java's Object.toString method. This allows using
;; the 'str' function with Rectangle instances.
;Object
;(toString [this]
;(declare str-rectangle) ; str-rectangle is declared later.
;(str-rectangle this)))
(defn get-column
  "Get values for the specified column"
  [index data]
  (map #(nth % index) data))
(defn wrap-with
  "Wraps a string in + sign"
  [wrapper data]
  (str wrapper data wrapper))
(defn build-border
  "Border for a formatted table"
  [colSizes]
  (->>
    (map #(repeat (+ 2 %) "-") colSizes)
    (map str/join)
    (str/join "+")
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
    (str/join "|")
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
    (str/join "\n")))

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
        fBody     (build-body colSizes rows)
        ]
    (str/join "\n" [border fHeader border fBody border])))


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
  (str/split s #","))
(defn read-csv
  "Reads a CSV file and splits into lines"
  [filename]
  (let [data (str/split-lines (slurp filename))
        header (first data)
        rows (rest data)]
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

(defn relation
  "This factory function creates a new relation object, taking the data from a table contained in a CSV file. The relation object must be an instance of a record (created with defrecord) or type (created with deftype). The parameter file-name must be a keyword naming a file with a .csv extension contained in the current directory."
  [file-name]
  (->>
    (read-csv (str (name file-name) ".csv"))
    create-record))

(deftest test-convert
  (is
    (= '(1 3 "ok" "ok2" "2ok" "string" 2345)
       (convert '("1" "3" "ok" "ok2" "2ok" "string" "2345")))))
(deftest test-widest
  (is (= 5 (widest '("a" "ab" "abc" "abcd" "abcde")))) ; 5 = abcde
  (is (= 3 (widest '("id" 1 2 12 20 300 22 111)))) ; 3 = 300 or 111
  (is (= 16 (widest '("Gwen Stacy" "Natalia Romanova" "Tony Stark" "Peggy Carter"
                            "Peter Parker" "Pepper Potts")))))

(run-tests)
