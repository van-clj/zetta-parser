(ns zetta.combinators
  (:refer-clojure :exclude [replicate])
  (:require [clojure.core :as core]
            [monads.core :refer [mplus return >> >>=]]
            [monads.util :refer [sequence-m]]
            [zetta.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser combinators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn <?>
  "Allows to add an error message to a given parser p."
  [p err-msg]
  (fn [input0 more0 err-fn0 ok-fn]
    (letfn [
      (err-fn [input0 more0 errors msg]
        #(err-fn0 input0 more0 (conj errors err-msg) err-msg))]
    (p input0 more0 err-fn ok-fn))))

(defn many
  "Applies zero or more times a parser p."
  [p]
  (do-parser
   h <- (mplus p (return []))
   (if (= h [])
     (return [])
     (do-parser
      t <- (many p)
      (return (cons h t))))))

(defn choice
  "Combinator that tries to parse the input using each of the given parsers,
  it will halt on the first parser that can successfuly parse the input."
  [ps]
  (with-parser
    (reduce mplus ps)))

(defn replicate
  "Apply the given parser 'p' 'n' times, returning every result."
  [n p]
  (with-parser
    (sequence-m (core/replicate n p))))

(defn option
  "Applies parser p to the input, if p fails then default-val is returned."
  [default-val p]
  (<|> p (return default-val)))

(defn many1
  "Applies one or more times a parser p."
  [p]
  (<$> cons p (many p)))

(defn around
  "Combinator that will apply the parser 'content' in between the parser
  'sep.'"
  [sep content]
  (*> sep (<* content sep)))

(defn sep-by1
  "Applies one or more times the parser p separated by parser s."
  [p s]
  (<$> cons
       p
       (<|> (*> s (sep-by1 p s))
            (return []))))

(defn sep-by
  "Applies zero or more times the parser p separated by parser s."
  [p s]
  (<|> (<$> cons
            p
            (<|> (*> s (sep-by1 p s))
                 (return [])))
       (return [])))

(defn many-till
  "Applies the parser p zero or more times until the parser end is successful."
  [p end]
  (<|> (*> end (return []))
       (>>= p (fn [h]
       (>>= (many-till p end) (fn [t]
       (return (cons h t))))))))

(defn skip-many
  "Skip zero or more applications of parser p."
  [p]
  (<|> (*> p (skip-many p))
       (return nil)))

(defn skip-many1
  "Skip one or more applications of parser p."
  [p]
  (*> p (skip-many p)))
