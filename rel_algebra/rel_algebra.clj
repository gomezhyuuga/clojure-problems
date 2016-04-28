;----------------------------------------------------------
; Activity: Project: Relational Algebra DSL
; Date: May 4, 2016.
; Author:
;          A01020319 Fernando Gómez Herrera
;----------------------------------------------------------

(ns rel-algebra
  (:require [clojure.string :as str]))

; UTILS
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

(defn relation
  "This factory function creates a new relation object, taking the data from a table contained in a CSV file. The relation object must be an instance of a record (created with defrecord) or type (created with deftype). The parameter file-name must be a keyword naming a file with a .csv extension contained in the current directory."
  [file-name]
  "")
