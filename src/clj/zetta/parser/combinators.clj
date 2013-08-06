(ns zetta.parser.combinators
  (:refer-clojure :exclude [replicate])
  ^:cljs-macro
  (:require [zetta.parser.macros :as zm])
  (:require
   ^{:cljs [cljs.core :as c]}
   [clojure.core :as c]
   #_(:cljs [zetta.parser.core :refer [Parser]])
   [zetta.parser.core :as z]
   [monads.core :as monad])
  ^:clj
  (:import [zetta.parser.core Parser]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser combinators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn <?>
  "Allows to add an error message to a given parser p."
  [p err-msg]
  (Parser.
   (fn -inner-<?> [input0 more0 err-fn0 ok-fn]
     (letfn [
             (err-fn [input0 more0 errors msg]
               #(err-fn0 input0 more0 (conj errors err-msg) msg))]
       (p input0 more0 err-fn0 ok-fn)))))

(defn many
  "Applies zero or more times a parser p."
  [p]
  (zm/do
    [h (z/<|> p (z/always []))
    :if (= h [])
    :then [ result (z/always []) ]
    :else [ t (many p)
            result (z/always (cons h t)) ]]
    result))

(defn choice
  "Combinator that tries to parse the input using each of the given parsers,
  it will halt on the first parser that can successfuly parse the input."
  [ps]
  (reduce z/<|> ps))

(defn replicate
  "Apply the given parser 'p' 'n' times, returning every result."
  [n p]
  (monad/seq (c/replicate n p)))

(defn option
  "Applies parser p to the input, if p fails then default-val is returned."
  [default-val p]
  (z/<|> p (z/always default-val)))

(defn many1
  "Applies one or more times a parser p."
  [p]
  (z/<$> cons p (many p)))

(defn around
  "Combinator that will apply the parser 'content' in between the parser
  'sep.'"
  [sep content]
  (zm/*> sep (zm/<* content sep)))

(defn sep-by1
  "Applies one or more times the parser p separated by parser s."
  [p s]
  (z/<$> cons
         p
         (z/<|> (zm/*> s (sep-by1 p s))
                (z/always []))))

(defn sep-by
  "Applies zero or more times the parser p separated by parser s."
  [p s]
  (z/<|> (z/<$> cons
                p
                (z/<|> (zm/*> s (sep-by1 p s))
                       (z/always [])))
         (z/always [])))

(defn many-till
  "Applies the parser p zero or more times until the parser end is successful."
  [p end]
  (z/<|> (zm/*> end (z/always []))
       (z/>>= p (fn [h]
       (z/>>= (many-till p end) (fn [t]
       (z/always (cons h t))))))))

(defn skip-many
  "Skip zero or more applications of parser p."
  [p]
  (z/<|> (zm/*> p (skip-many p))
         (z/always nil)))

(defn skip-many1
  "Skip one or more applications of parser p."
  [p]
  (zm/*> p (skip-many p)))
