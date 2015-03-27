(ns zetta.examples.clojure
  ^{:doc "A parser used on this project's README file"}
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [zetta.core
             :refer :all]
            [zetta.combinators
             :as pc]
            [zetta.parser.seq
             :as pseq]
            [zetta.parser.string
             :as pstr]))

(defrecord Actor [name movies])
(defrecord Doctor [name patients])
(defrecord Programmer [name programs])

(def parse-movie
  (do-parser
   pseq/skip-spaces
   (pstr/take-till #(or (Character/isWhitespace %)
                        (= % \,)))))

(def parse-patient
  (do-parser
   pseq/skip-spaces
   first-name <- (pstr/take-till #(Character/isWhitespace %))
   pseq/skip-spaces
   last-name  <- (pstr/take-till #(or (Character/isWhitespace %)
                                      (= % \,)))
   (always [first-name last-name])))

(def parse-person-year
  (do-parser
   pseq/skip-spaces
   year <- pseq/number
   (if (>= year 1900)
     (always year)
     (fail-parser "Expecting year to be greater than 1900"))))

(def parse-program
  (do-parser
   pseq/skip-spaces
   program-year <- parse-person-year
   pseq/skip-spaces
   (pseq/char \")
   program-title <- (pstr/take-till #(= % \"))
   (pseq/char \")
   (always [program-year program-title])))


(def parse-professional
  (do-parser
    pseq/skip-spaces
    name <- (pstr/take-till #(Character/isSpace %))
    pseq/skip-spaces
    profession <- (pstr/take-till #(Character/isSpace %))
    (always [name profession])

    (cond
      (= profession "actor")
      (do-parser
       movies <- (pc/sep-by1 parse-movie (pseq/char \,))
       (always (Actor. name movies)))

      (= profession "doctor")
      (do-parser
       patients <- (pc/sep-by1 parse-patient (pseq/char \,))
       (always (Doctor. name patients)))

      (= profession "programmer")
      (do-parser
       programs <- (pc/sep-by1 parse-program (pseq/char \,))
       (always (Programmer. name programs)))

      :else
      (fail-parser (str "Invalid profession: " profession)))))
