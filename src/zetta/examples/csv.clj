(ns zetta.examples.csv
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core])
  (:require [clojure.java.io :as io])

  (:use zetta.core)
  (:use
    [zetta.combinators
      :only
      [sep-by1 around many many1 choice]]

    [zetta.parser.seq
      :only
      [char not-char spaces eol end-of-input]]))

(defrecord CSVFile [titles values])

(def csv-sep
  (char \,))
           
(def csv-key
  (with-parser
    (<$> apply-str 
         (many1 
           (around spaces (not-char #{\, \newline}))))))

(def csv-entry
  (with-parser
     (<* (sep-by1 csv-key
                  csv-sep)
         (<|> eol end-of-input))))

(def csv-file
  (with-parser
    (<$> #(CSVFile. %1 %2)
         csv-entry
         (many csv-entry))))

