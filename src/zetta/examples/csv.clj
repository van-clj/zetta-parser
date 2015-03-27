(ns zetta.examples.csv
  ^{ :doc "Naive CSV parser" }
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [zetta.core :refer :all]
            [zetta.combinators
             :refer [sep-by1 around many many1 choice]]
            [zetta.parser.seq
             :refer [char not-char spaces eol end-of-input]]))

(defrecord CSVFile [titles values])

(def csv-sep
  (char \,))

(def csv-key
  (<$> str/join
       (many1
         (around spaces (not-char #{\, \newline})))))

(def csv-entry
  (<* (sep-by1 csv-key
               csv-sep)
       (<|> eol end-of-input)))

(def csv-file
  (<$> #(CSVFile. %1 %2)
       csv-entry
       (many csv-entry)))

;; Run this with:
;; (parse-once csv-file "first,last\nJohn,Doe")
