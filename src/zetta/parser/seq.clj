(ns zetta.parser.seq
  (:refer-clojure
   :exclude [ensure get take take-while char some replicate])
  (:require [clojure.core :as core]
            [clojure.string :as str])

  (:use zetta.core)
  (:use zetta.combinators))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- span [pred xs]
  ((core/juxt #(core/take-while pred %) #(core/drop-while pred %)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Parsers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def demand-input
  "Basic parser that will ensure the request of more
   input via a continuation."
  (fn [input0 more0 err-fn0 ok-fn0]
    (if (complete? more0)
      (err-fn0 input0 more0 ["demand-input"] "not enough input")
      (letfn [
        (err-fn [input more]
          (err-fn0 input more ["demand-input"] "not enough input"))
        (ok-fn [input more]
          (ok-fn0 input more nil))]
      (prompt input0 more0 err-fn ok-fn)))))

(def want-input?
  "This parser always succeeds.  It returns 'true' if any
   input is available either immediately or on demand, and
   'false' if the end of all input has been reached."
  (fn [input0 more0 _err-fn ok-fn0]
    (cond
      (not (empty? input0)) (ok-fn0 input0 more0 true)
      (complete? more0) (ok-fn0 input0 more0 false)
      :else
        (letfn [(err-fn [input more] (ok-fn0 input more false))
                (ok-fn [input more] (ok-fn0 input more true))]
        (prompt input0 more0 err-fn ok-fn)))))

(defn ensure
  "If at least 'n' items of input are available, return the current
  input, otherwise fail."
  [n]
  (fn [input0 more0 err-fn ok-fn]
    (if (>= (count input0) n)
      (ok-fn input0 more0 input0)
      (with-parser
        ((>> demand-input (ensure n)) input0 more0 err-fn ok-fn)))))

(def get
  (fn [input0 more0 _err-fn ok-fn]
    (ok-fn input0 more0 input0)))

(defn put [s]
  (fn [_input0 more0 _err-fn ok-fn]
    (ok-fn s more0 nil)))

(defn satisfy?
  "The parser 'satisfy pred' succeeds for any item for which the
   predicate 'pred' returns 'true'. Returns the item that is actually
   parsed."
  [pred]
  (do-parser
    [input     (ensure 1)
     :let  [item (first input)]
     :if (pred item)
       :then [
         _ (put (rest input))
       ]
       :else [
        _ (fail-parser "satisfy?")
       ]]
    item))

(defn skip
  "The parser 'skip pred' succeeds for any item for which the predicate
   'pred' returns 'true'."
  [pred]
  (do-parser
    [input (ensure 1)
     :if (pred (first input))
       :then [_ (put (rest input))]
       :else [_ (fail-parser "skip")]]
     nil))

(defn take-with
  "Matches 'n' items of input, but succeed only if the predicate
  'pred' returns 'true' on the parsed input. The matched input is returned
  as a seq."
  [n pred]
  (do-parser
    [input (ensure n)
     :let [[h t] (split-at n input)]
     :if (pred h)
       :then [_ (put t)]
       :else [_ (fail-parser "take-with")]]
     h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; High Level Parsers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn take
  "Matches exactly 'n' items from input. Returnes the matched input as a seq."
  [n]
  (take-with n (constantly true)))

(defn string
  "Parses a sequence of items that identically match
   a given string 's'. Returns the parsed string.  This parser
   consumes no input if it fails (even with a partial match)."
  [s]
  (let [ch-vec (vec s)]
    (<$> str/join
         (take-with (count s) #(= ch-vec %)))))

(defn skip-while
  "A parser that skips input for as long as 'pred' returns 'true'."
  [pred]
  (let [skip-while-loop (do-parser
             [input0 get
              :let [input (drop-while pred input0)]
              _ (put input)
              :if (empty? input)
              :then [
                input-available? want-input?
                :if input-available?
                :then [_ (skip-while pred)]
                :else []
              ]
              :else []]
              nil)]
     skip-while-loop))

(defn take-while
  "Matches input as long as pred returns 'true', and return
   the consumed input as a seq.

   This parser does not fail.  It will return an empty seq if the
   predicate returns 'false' on the first token of input.

   Note: Because this parser does not fail, do not use it with
   combinators such as 'many', because such parsers loop until a
   failure occurs.  Careless use will thus result in an infinite loop."
  [pred]
  (letfn [
    (take-while-loop [acc]
      (do-parser
        [input0 get
         :let [[pre post] (span pred input0)]
         _ (put post)
         :if (empty? post)
           :then [
             input-available? want-input?
             :if input-available?
               :then [result (take-while-loop (conj acc pre))]
               :else [result (always (conj acc pre))]
           ]
           :else [result (m-result (conj acc pre))]]
           result))]
  (<$> (comp #(apply core/concat %) core/reverse)
       (take-while-loop []))))

(defn take-till
  "Matches input as long as 'pred' returns 'false'
  (i.e. until it returns 'true'), and returns the consumed input as a seq.

  This parser does not fail.  It will return an empty seq if the
  predicate returns 'true' on the first item from the input.

  Note: Because this parser does not fail, do not use it with
  combinators such as 'many', because such parsers loop until a
  failure occurs.  Careless use will thus result in an infinite loop."
  [pred]
  (take-while (complement pred)))

(def take-rest
  "Returns the rest of the seqs that are given to the parser,
  the result will be a seqs of seqs where the number of seqs
  from the first level will represent the number of times a
  continuation was used to continue the parse process."
  (letfn [
    (take-rest-loop [acc]
      (do-parser
        [input-available? want-input?
         :if input-available?
           :then [
             input get
             _ (put [])
             result (take-rest-loop (conj acc input))
           ]
           :else [
             result (always (reverse acc))
           ]]
          result))]
  (take-rest-loop [])))

(defn take-while1
  "Matches input as long as pred returns 'true'. This parser returns
   the consumed input in a seq.

   This parser will fail if a first match is not accomplished."
  [pred]
  (do-parser
    [input get
     :if (empty? input)
       :then [_ demand-input]
       :else []
     input get
     :let [[pre post] (span pred input)]
     :if (empty? pre)
       :then [_ (fail-parser "take-while1")]
       :else [_ (put post)]
     :if (empty? post)
       :then [remainder (take-while pred)
              result (always (concat pre remainder))]
       :else [result (always pre)]]
     result))

(def any-token
  "Matches any element from the input seq, it
  will return the parsed element from the seq."
  (satisfy? (constantly true)))

(defn char
  "Matches only a token that is equal to character
  'c', the character is returned."
  [c]
  (cond
    (set? c)
      (<?> (satisfy? #(contains? c %))
           (str "failed parser char: " c))
    :else
      (<?> (satisfy? #(= % c))
           (str "failed parser char: " c))))


(defn not-char
  "Matches only a token that is not equal to character
  'c', the character is returned."
  [c]
  (cond
    (set? c)
      (<?> (satisfy? #(not (contains? c %)))
           (str c))
    :else
      (<?> (satisfy? #(not (= % c)))
           (str c))))

(def letter
  "Matches any character that is considered a letter,
  it uses 'Character/isLetter' internally. This parser will return
  the matched character."
  (satisfy? #(Character/isLetter %)))

(def word
  (<$> str/join (many1 letter)))

(def digit
  "Matches any character that is considered a digit, it uses
  'Character/isDigit' internally. This parser will return the
  matched character."
  (satisfy? #(Character/isDigit %)))

; Using the clojure LispReader, code found on stackoverflow
; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure
(let [m (.getDeclaredMethod clojure.lang.LispReader
                            "matchNumber"
                            (into-array [String]))]
  (.setAccessible m true)
  (defn- read-number [s]
    (.invoke m clojure.lang.LispReader (into-array [s]))))

(def double-or-long
  (letfn [
    (dot-or-digit [] (do-parser [
      c  (<|> digit (char \.))
      :if (= c \.)
      :then
        [result (<$> #(cons \. %) (many1 digit))]
      :else
        [result (<$> #(cons c %) (<|> (dot-or-digit)
                                      (always [])))]]
      result))]
    (<$> cons digit (dot-or-digit))))

(def number
  "Matches one or more digit characters and returns the number parsed
  in base 10."
  (<$> (comp read-number str/join)
       double-or-long))

(def whitespace
  "Matches any character that is considered a whitespace,
  it uses 'Character/isWhitespace' internally. This parser
  returns the whitespace character."
  (satisfy? #(Character/isWhitespace %)))

(def space
  "Matches any character that is equal to the character
  \\space. This parser returns the \\space character."
  (char \space))

(def spaces
  "Matches many spaces."
  (many space))

(def skip-spaces
  "Skips many spaces."
  (skip-many space))

(def skip-whitespaces
  "Skips many whitespaces."
  (skip-many whitespace))

(def end-of-input
  "Matches only when the end-of-input has been reached, otherwise
  it fails. This parser returns a nil value."
  (fn [input0 more0 err-fn0 ok-fn0]
    (if (empty? input0)
      (if (complete? more0)
        (ok-fn0 input0 more0 nil)
        (letfn [
          (err-fn [input1 more1 _ _]
            (add-parser-stream input0 more0 input1 more1
                               (fn [input2 more2]
                                  (ok-fn0 input2 more2 nil))))
          (ok-fn [input1 more1 _]
            (add-parser-stream input0 more0 input1 more1
                               (fn [input2 more2]
                                  (err-fn input2 more2 [] "end-of-input"))))]
        (demand-input input0 more0 err-fn ok-fn)))
      (err-fn0 input0 more0 [] "end-of-input"))))

(def at-end?
  "Parser that never fails, it returns 'true' when the end-of-input
  is reached, 'false' otherwise."
  (<$> not want-input?))

(def eol
  "Parser that matches different end-of-line characters/sequences.
  This parser returns a nil value."
  (<|> (*> (char \newline) (always nil))
       (*> (string "\r\n") (always nil))))


