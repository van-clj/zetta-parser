(ns zetta.combinators
  (:refer-clojure :exclude [do replicate])
  (:require [zetta.parser.macros :as macros])
  (:require ^{:cljs [cls.core :as core]}
            [clojure.core :as core]
            [monads.core :as monad]
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
        #(err-fn0 input0 more0 (conj errors err-msg) msg))]
    (p input0 more0 err-fn0 ok-fn))))

(defn many
  "Applies zero or more times a parser p."
  [p]
  (macros/do
    [h (<|> p (always []))
    :if (= h [])
    :then [ result (always []) ]
    :else [ t (many p)
            result (always (cons h t)) ]]
    result))

(defn choice
  "Combinator that tries to parse the input using each of the given parsers,
  it will halt on the first parser that can successfuly parse the input."
  [ps]
  (reduce <|> ps))

(defn replicate
  "Apply the given parser 'p' 'n' times, returning every result."
  [n p]
  (monad/seq (core/replicate n p)))

(defn option
  "Applies parser p to the input, if p fails then default-val is returned."
  [default-val p]
  (<|> p (always default-val)))

(defn many1
  "Applies one or more times a parser p."
  [p]
  (<$> cons p (many p)))

(defn around
  "Combinator that will apply the parser 'content' in between the parser
  'sep.'"
  [sep content]
  (macros/*> sep (macros/<* content sep)))

(defn sep-by1
  "Applies one or more times the parser p separated by parser s."
  [p s]
  (<$> cons
       p
       (<|> (macros/*> s (sep-by1 p s))
            (always []))))

(defn sep-by
  "Applies zero or more times the parser p separated by parser s."
  [p s]
  (<|> (<$> cons
            p
            (<|> (macros/*> s (sep-by1 p s))
                 (always [])))
       (always [])))

(defn many-till
  "Applies the parser p zero or more times until the parser end is successful."
  [p end]
  (<|> (macros/*> end (always []))
       (>>= p (fn [h]
       (>>= (many-till p end) (fn [t]
       (always (cons h t))))))))

(defn skip-many
  "Skip zero or more applications of parser p."
  [p]
  (<|> (macros/*> p (skip-many p))
       (always nil)))

(defn skip-many1
  "Skip one or more applications of parser p."
  [p]
  (macros/*> p (skip-many p)))
