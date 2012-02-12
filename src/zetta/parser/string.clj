(ns zetta.parser.string
  (:refer-clojure :exclude [take take-while])
  (:require [clojure.core :as core]
            [clojure.string :as str])

  (:use     [zetta.core])
  (:require [zetta.parser.seq :as pseq]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; High Level Parsers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn take-with
  "Matches 'n' items of input, but succeed only if the predicate
  'pred' returns 'true' on the parsed input. The matched input is returned
  as a string."
  [n pred]
  (<$> str/join
       (pseq/take-with n pred)))

(defn take
  "Matches exactly 'n' items from input. Returnes the matched input as
  a string."
  [n]
  (take-with n (constantly true)))

(defn string
  "Parses a sequence of items that identically match
   a given string 's'. Returns the parsed string.  This parser
   consumes no input if it fails (even with a partial match)."
  [s]
  (take-with (count s) #(= s %)))

(defn take-while
  "Matches input as long as pred returns 'true', and return
   the consumed input as a string.

   This parser does not fail.  It will return an empty seq if the
   predicate returns 'false' on the first token of input.

   Note: Because this parser does not fail, do not use it with
   combinators such as 'many', because such parsers loop until a
   failure occurs.  Careless use will thus result in an infinite loop."
  [pred]
  (<$> str/join
       (pseq/take-while pred)))

(defn take-till
  "Matches input as long as 'pred' returns 'false'
  (i.e. until it returns 'true'), and returns the consumed input as a seq.

  This parser does not fail.  It will return an empty string if the
  predicate returns 'true' on the first item from the input.

  Note: Because this parser does not fail, do not use it with
  combinators such as 'many', because such parsers loop until a
  failure occurs.  Careless use will thus result in an infinite loop."
  [pred]
  (take-while (complement pred)))

(def take-rest
  "Returns the rest of the seqs that are given to the parser,
  the result will be a seqs of strings where the number of seqs
  from the first level will represent the number of times a
  continuation was used to continue the parse process."
  (<$> #(map str/join %) pseq/take-rest))

(defn take-while1
  "Matches input as long as pred returns 'true'. This parser returns
   the consumed input in a string.

   This parser will fail if a first match is not accomplished."
  [pred]
  (<$> str/join (pseq/take-while1 pred)))

