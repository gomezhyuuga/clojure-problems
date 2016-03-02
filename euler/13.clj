(ns rescue.core
  (:require [clojure.java.io :as io] ))

(def data (io/resource "bignum.txt"))

(println (slurp data))
