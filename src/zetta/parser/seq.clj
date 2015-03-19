(ns zetta.parser.seq
  (:refer-clojure
   :exclude [ensure get take take-while char some replicate])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [monads.core :refer [lift-m return run-monad >>]]
            [zetta.core :refer :all]
            [zetta.combinators :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Utility Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- span
  [pred xs]
  ((core/juxt #(core/take-while pred %) #(core/drop-while pred %)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Basic Parsers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def demand-input
  "Basic parser that will ensure the request of more input via a continuation."
  (fn [input0 more0 err-fn0 ok-fn0]
    (if (complete? more0)
      #(err-fn0 input0 more0 ["demand-input"] "not enough input")
      (letfn [
        (err-fn [input more]
          #(err-fn0 input more ["demand-input"] "not enough input"))
        (ok-fn [input more]
          #(ok-fn0 input more nil))]
      (prompt input0 more0 err-fn ok-fn)))))

(def want-input?
  "Parser that returns `true` if any input is available either immediately or
  on demand, and `false` if the end of all input has been reached.

  **WARNING**: This parser return succeeds."
  (fn [input0 more0 _err-fn ok-fn0]
    (cond
      (not (empty? input0)) #(ok-fn0 input0 more0 true)
      (complete? more0) #(ok-fn0 input0 more0 false)
      :else
        (letfn [(err-fn [input more] #(ok-fn0 input more false))
                (ok-fn [input more] #(ok-fn0 input more true))]
        (prompt input0 more0 err-fn ok-fn)))))

(defn ensure
  "If at least `n` items of input are available, return the current input,
  otherwise fail."
  [n]
  (fn [input0 more0 err-fn ok-fn]
    (if (>= (count input0) n)
      #(ok-fn input0 more0 input0)
      ((run-monad parser-m (>> demand-input (ensure n)))
       input0 more0 err-fn ok-fn))))

(def get
  "Returns the input given in the `zetta.core/parse` function."
  (fn [input0 more0 _err-fn ok-fn]
    #(ok-fn input0 more0 input0)))

(defn put [s]
  "Sets a (possibly modified) input into the parser state."
  (fn [_input0 more0 _err-fn ok-fn]
    #(ok-fn s more0 nil)))

(defn satisfy?
  "Parser that succeeds for any item for which the predicate `pred` returns
  `true`. Returns the item that is actually parsed."
  [pred]
  (do-parser
   input <- (ensure 1)
   let item = (first input)
   _ <- (if (pred item)
          (put (rest input))
          (fail-parser "satisfy?"))
   (return item)))

(defn skip
  "Parser that succeeds for any item for which the predicate `pred`, returns
  `nil`."
  [pred]
  (do-parser
   input <- (ensure 1)
   (if (pred (first input))
     (put (rest input))
     (fail-parser "skip"))))

(defn take-with
  "Parser that matches `n` items of input, but succeed only if the predicate
  `pred` returns `true` on the parsed input. The matched input is returned
  as a seq."
  [n pred]
  (do-parser
   input <- (ensure n)
   let [h t] = (split-at n input)
   _ <- (if (pred h)
          (put t)
          (fail-parser "take-with"))
   (return h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## High Level Parsers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn take
  "Parser that matches exactly `n` items from input. Returnes the matched
  input as a seq."
  [n]
  (take-with n (constantly true)))

(defn string
  "Parses a sequence of items that identically match a given string `s`.
  Returns the parsed string. This parser consumes no input if it fails (even
  with a partial match)."
  [#^java.lang.String s]
  (let [ch-vec (vec s)]
    (<$> str/join
         (take-with (count s) #(= ch-vec %)))))

(defn skip-while
  "Parser that skips input for as long as `pred` returns `true`."
  [pred]
  (let [skip-while-loop
        (do-parser
         input0 <- get
         let input = (drop-while pred input0)
         _ <- (put input)
         (if (empty? input)
           (do-parser
            input-available? <- want-input?
            (if input-available?
              (skip-while pred)
              (return nil)))
           (return nil)))]
     skip-while-loop))

(defn take-while
  "Parser that matches input as long as pred returns `true`, and return
   the consumed input as a seq.

   This parser does not fail.  It will return an empty seq if the predicate
   returns `false` on the first token of input.

   **WARNING**: Because this parser does not fail, do not use it with
   combinators such as `many`, because such parsers loop until a failure
   occurs. Careless use will thus result in an infinite loop."
  [pred]
  (letfn [
    (take-while-loop [acc]
      (do-parser
       input0 <- get
       let [pre post] = (span pred input0)
       _ <- (put post)
       (if (empty? post)
         (do-parser
          input-available? <- want-input?
          (if input-available?
            (take-while-loop (conj acc pre))
            ;; else
            (return (conj acc pre))))
         ;; else
         (return (conj acc pre)))))]
    (with-parser
      (lift-m (comp #(apply core/concat %) core/reverse)
              (take-while-loop [])))))

(defn take-till
  "Matches input as long as `pred` returns `false`
  (i.e. until it returns `true`), and returns the consumed input as a seq.

  This parser does not fail.  It will return an empty seq if the predicate
  returns `true` on the first item from the input.

  **WARNING**: Because this parser does not fail, do not use it with combinators
  such as `many`, because such parsers loop until a failure occurs. Careless
  use will thus result in an infinite loop."
  [pred]
  (take-while (complement pred)))

(def take-rest
  "Parser that returns the rest of the seqs that are given to the parser,
  the result will be a seqs of seqs where the number of seqs
  from the first level will represent the number of times a
  continuation was used to continue the parse process."
  (letfn [
    (take-rest-loop [acc]
      (do-parser
       input-available? <- want-input?
       (if input-available?
         (do-parser
          input <- get
          _ <- (put [])
          (take-rest-loop (conj acc input)))
         ;; else
         (return (reverse acc)))))]
  (take-rest-loop [])))

(defn take-while1
  "Parser that matches input as long as pred returns `true`. This parser
   returns the consumed input in a seq.

   This parser will fail if a first match is not accomplished."
  [pred]
  (do-parser
   input <- get
   _ <- (if (empty? input)
          demand-input
          (return nil))

   input <- get
   let [pre post] = (span pred input)
   _ <- (if (empty? pre)
          (fail-parser "take-while1")
          (put post))

   (if (empty? post)
     (do-parser
      remainder <- (take-while pred)
      (return (concat pre remainder)))
     ;; else
     (return pre))))

(def any-token
  "Parser that matches any element from the input seq, it will return the
  parsed element from the seq."
  (satisfy? (constantly true)))

(defn char
  "Parser that matches only a token that is equal to character `c`, the
  character is returned."
  [#^java.lang.Character c]
  (cond
    (set? c)
      (<?> (satisfy? #(contains? c %))
           (str "failed parser char: " c))
    :else
      (<?> (satisfy? #(= % c))
           (str "failed parser char: " c))))

(defn not-char
  "Parser that matches only an item that is not equal to character `c`, the
  item is returned."
  [#^java.lang.Character c]
  (cond
    (set? c)
      (<?> (satisfy? #(not (contains? c %)))
           (str c))
    :else
      (<?> (satisfy? #(not (= % c)))
           (str c))))

(def whitespace
  "Parser that matches any character that is considered a whitespace, it uses
  `Character/isWhitespace` internally. This parser returns the whitespace
  character."
  (satisfy? #(Character/isWhitespace #^java.lang.Character %)))

(def space
  "Parser that matches any character that is equal to the character `\\space`.
  This parser returns the `\\space` character."
  (char \space))

(def spaces
  "Parser that matches many spaces. Returns a seq of space characters"
  (many space))

(def skip-spaces
  "Parser that skips many spaces. Returns `nil`."
  (skip-many space))

(def skip-whitespaces
  "Parser that skips many whitespaces. Returns `nil`."
  (skip-many whitespace))

(def letter
  "Parser that matches any character that is considered a letter, it uses
  `Character/isLetter` internally. This parser will return the matched
  character."
  (satisfy? #(Character/isLetter ^java.lang.Character %)))

(def word
  "Parser that matches a word, e.g `(many1 letter)`, returns the parsed word."
  (<$> str/join
       (many1 letter)))

(def digit
  "Parser that matches any character that is considered a digit, it uses
  `Character/isDigit` internally. This parser will return the matched digit
  character."
  (satisfy? #(Character/isDigit #^java.lang.Character %)))

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
    (digit-or-dot []
      (do-parser
       h <- (join-parsers digit (char \.))
       t <- (if (= h \.)
              (many digit)
              (join-parsers (digit-or-dot) (return [])))
       (return (cons h t))))]
    (do-parser
     h <- digit
     t <- (join-parsers (digit-or-dot) (return []))
     (return (cons h t)))))

(def number
  "Parser that matches one or more digit characters and returns a number in
  base 10."
  (<$> (comp read-number str/join)
       double-or-long))

(def end-of-input
  "Parser that matches only when the end-of-input has been reached, otherwise
  it fails. Returns a nil value."
  (fn [input0 more0 err-fn0 ok-fn0]
    (if (empty? input0)
      (if (complete? more0)
        #(ok-fn0 input0 more0 nil)
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
      #(err-fn0 input0 more0 [] "end-of-input"))))

(def at-end?
  "Parser that never fails, it returns `true` when the end-of-input
  is reached, `false` otherwise."
  (<$> not want-input?))

(def eol
  "Parser that matches different end-of-line characters/sequences.
  This parser returns a nil value."
  (<|> (*> (char \newline) (return nil))
       (*> (string "\r\n") (return nil))))
