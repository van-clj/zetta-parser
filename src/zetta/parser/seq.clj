(ns zetta.parser.seq
  (:refer-clojure
   :exclude [ensure get take take-while char some replicate])
  (:require [clojure.core :as core])

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
  (fn [i0 m0 ff sf]
    (if (complete? m0)
      (ff i0 m0 ["demand-input"] "not enough input")
      (letfn [
        (new-ff [i m]
          (ff i m ["demand-input"] "not enough input"))
        (new-sf [i m]
          (sf i m nil))]
      (prompt i0 m0 new-ff new-sf)))))

(def want-input?
  "This parser always succeeds.  It returns 'true' if any
   input is available either immediately or on demand, and
   'false' if the end of all input has been reached."
  (fn [i0 m0 _ff sf]
    (cond
      (not (empty? i0)) (sf i0 m0 true)
      (= m0 complete)   (sf i0 m0 false)
      :else
        (letfn [(new-ff [i m] (sf i m false))
                (new-sf [i m] (sf i m true))]
        (prompt i0 m0 new-ff new-sf)))))

(defn ensure
  "If at least 'n' items of input are available, return the current
  input, otherwise fail."
  [n]
  (fn [i0 m0 ff sf]
    (if (>= (count i0) n)
      (sf i0 m0 i0)
      (with-parser
        ((>> demand-input (ensure n)) i0 m0 ff sf)))))

(def ^:private get
  (fn [i0 m0 _ff sf]
    (sf i0 m0 i0)))

(defn- put [s]
  (fn [_i0 m0 _ff sf]
    (sf s m0 nil)))

(defn satisfy?
  "The parser 'satisfy pred' succeeds for any item for which the
   predicate 'pred' returns 'true'. Returns the item that is actually
   parsed."
  [pred]
  (do-parser
    [s     (ensure 1)
     :let  [w (first s)]
     :if (pred w)
       :then [
         _ (put (rest s))
       ]
       :else [
        _ (fail-parser "satisfy?")
       ]]
    w))

(defn skip
  "The parser 'skip pred' succeeds for any item for which the predicate
   'pred' returns 'true'."
  [pred]
  (do-parser
    [s (ensure 1)
     :if (pred (first s))
       :then [_ (put (rest s))]
       :else [_ (fail-parser "skip")]]
     nil))

(defn take-with
  "Matches 'n' items of input, but succeed only if the predicate
  'pred' returns 'true' on the parsed input. The matched input is returned
  as a seq."
  [n pred]
  (do-parser
    [s (ensure n)
     :let [[h t] (split-at n s)]
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
  (with-parser
    (take-with n (constantly true))))

(defn string
  "Parses a sequence of items that identically match
   a given string 's'. Returns the parsed string.  This parser
   consumes no input if it fails (even with a partial match)."
  [s]
  (let [vecs (vec s)]
    (with-parser
      (<$> (partial apply str)
           (take-with (count s) #(= vecs %))))))

(defn skip-while
  "A parser that skips input for as long as 'pred' returns 'true'."
  [pred]
  (let [go (do-parser
            [t0 get
             :let [t (drop-while pred t0)]
             _ (put t)
             :if (empty? t)
             :then [
               input want-input?
               :if input
               :then [_ (skip-while pred)]
               :else []
             ]
             :else []]
             nil)]
     go))

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
    (go [acc]
      (do-parser
        [t0 get
         :let [[h t] (span pred t0)]
         _ (put t)
         :if (empty? t)
           :then [
             input want-input?
             :if input
               :then [result (go (conj acc h))]
               :else [result (m-result (conj acc h))]
           ]
           :else [result (m-result (conj acc h))]]
           result))]
  (do-parser
    [result (go [])]
    (->> result core/reverse (apply core/concat)))))

(defn take-till
  "Matches input as long as 'pred' returns 'false'
  (i.e. until it returns 'true'), and returns the consumed input as a seq.

  This parser does not fail.  It will return an empty seq if the
  predicate returns 'true' on the first item from the input.

  Note: Because this parser does not fail, do not use it with
  combinators such as 'many', because such parsers loop until a
  failure occurs.  Careless use will thus result in an infinite loop."
  [pred]
  (with-parser
    (take-while (complement pred))))

(def take-rest
  "Returns the rest of the seqs that are given to the parser,
  the result will be a seqs of seqs where the number of seqs
  from the first level will represent the number of times a
  continuation was used to continue the parse process."
  (letfn [
    (go [acc]
      (do-parser
        [input want-input?
         :if input
           :then [
             s get
             _ (put [])
             result (go (conj acc s))
           ]
           :else [
             result (m-result (reverse acc))
           ]]
          result))]
  (go [])))

(defn take-while1
  "Matches input as long as pred returns 'true'. This parser returns
   the consumed input in a seq.

   This parser will fail if a first match is not accomplished."
  [pred]
  (do-parser
    [input-checker get
     :if (empty? input-checker)
       :then [_ demand-input]
       :else []
     current-input get
     :let [[h t] (span pred current-input)]
     :if (empty? h)
       :then [_ (fail-parser "take-while-1")]
       :else [_ (put t)]
     :if (empty? t)
       :then [remainder (take-while pred)
              result (m-result (concat h remainder))]
       :else [result (m-result h)]]
     result))

(def any-token
  "Matches any element from the input seq, it
  will return the parsed element from the seq."
  (with-parser
    (satisfy? (constantly true))))

(defn char
  "Matches only a token that is equal to character
  'c', the character is returned."
  [c]
  (with-parser
    (cond
      (set? c)
        (<?> (satisfy? #(contains? c %))
             (str c))
      :else
        (<?> (satisfy? #(= % c))
             (str c)))))


(defn not-char
  "Matches only a token that is not equal to character
  'c', the character is returned."
  [c]
  (with-parser
    (cond
      (set? c)
        (<?> (satisfy? #(not (contains? c %)))
             (str c))
      :else
        (<?> (satisfy? #(not (= % c)))
             (str c)))))

(def letter
  "Matches any character that is considered a letter,
  it uses 'Character/isLetter' internally. This parser will return
  the matched character."
  (with-parser
    (satisfy? #(Character/isLetter %))))

(def word
  (with-parser
    (<$> apply-str (many1 letter))))

(def digit
  "Matches any character that is considered a digit, it uses 
  'Character/isDigit' internally. This parser will return the 
  matched character."
  (with-parser
    (satisfy? #(Character/isDigit %))))

(def number
  "Matches one or more digit characters and returns the number parsed
  in base 10."
  (with-parser
    (<$> (comp #(Integer/parseInt %) #(apply str %))
         (many1 digit))))

(def whitespace
  "Matches any character that is considered a whitespace,
  it uses 'Character/isWhitespace' internally. This parser 
  returns the whitespace character."
  (with-parser
    (satisfy? #(Character/isWhitespace %))))

(def space
  "Matches any character that is equal to the character
  \\space. This parser returns the \\space character."
  (with-parser
    (char \space)))

(def spaces
  (with-parser
    (many space)))

(def end-of-input
  "Matches only when the end-of-input has been reached, otherwise
  it fails. This parser returns a nil value."
  (fn [i0 m0 ff sf]
    (if (empty? i0)
      (if (complete? m0)
        (sf i0 m0 nil)
        (letfn [
          (new-ff [i1 m1 _ _]
            (add-parser-stream i0 m0 i1 m1
                               (fn [i2 m2] (sf i2 m2 nil))))
          (new-sf [i1 m1 _]
            (add-parser-stream i0 m0 i1 m1
                               (fn [i2 m2] (ff i2 m2 [] "end-of-input"))))]
        (demand-input i0 m0 new-ff new-sf)))
      (ff i0 m0 [] "end-of-input"))))

(def at-end?
  "Parser that never fails, it returns 'true' when the end-of-input
  is reached, 'false' otherwise."
  (with-parser
    (<$> not want-input?)))

(def eol
  "Parser that matches different end-of-line characters/sequences.
  This parser returns a nil value."
  (with-parser
    (<|> (*> (char \newline) (m-result nil))
         (*> (string "\r\n") (m-result nil)))))


