(ns zetta.core
  (:use [clojure.algo.monads
         :only
         [defmonad defmonadfn domonad with-monad m-seq]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser result types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ResultDone [remainder result])
(defrecord ResultFailure [remainder stack msg])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser result query functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def
  ^{:doc "Returns true when the parser requires more input
         in order to complete."}
  partial? fn?)

(defn done?
  "Returns true when the parser has successfuly parsed the
  given input."
  [result]
  (instance? ResultDone result))

(defn failure?
  "Returns true when the parser has failed when parsing the
  given input."
  [result]
  (instance? ResultFailure result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser more-input constants
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def
  ^{:doc "Constant value that indicates if the stream
         of the parser is complete (no more input)."}
  complete ::complete)

(def
  ^{:doc "Constant value that indicates if the stream
         of the parser is incomplete (more input to come)."}
  incomplete ::incomplete)

(defn complete?
  "Test if equal to the complete constant or not."
  [m] (= m complete))

(defn incomplete?
  "Test if equal to the incomplete constant or not."
  [m] (= m incomplete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad & Applicative utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Haskell like operators

(defmonadfn >>=
  "Equivalent to the m-bind function for monads."
  [m1 f]
  (m-bind m1 f))

(defn- bind-ignore-step [mresult m1]
  `(>>= ~mresult (fn [~'_]
   ~m1)))

(defmacro >>
  "Composes two or more monadic values returning the
   result of the last one."
  [& more]
  (reduce bind-ignore-step more))

;; Haskell Applicative operators

(defmacro *>
  "Composes two or more applicative functors, returning the
   result value of the last one."
  [& more]
  `(>> ~@more))

(defmacro <*
  "Composes two or more applicative functors, returning the
   result value of the first one."
  [& more]
  (let [step (first more)
        steps (rest more)]
  `(>>= ~step (fn [~'result#]
   (>> ~(reduce bind-ignore-step steps)
        (~'m-result ~'result#))))))

(defmonadfn <$>
  "Maps the function f to the results of the given applicative functors,
  each applicative functor result value is going to be a parameter for
  function f.

  Example:
    (<$> + number number)

  Where number is parser that will return a number from the input stream."
  [f & more]
  (>>= (m-seq more) (fn [params]
  (m-result (apply f params)))))

(defmonadfn <|>
  "Equivalent to the m-plus function for monads."
  [m1 m2]
  (m-plus m1 m2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser Monad utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- concat-more
  "Monoid mappend function for the More values complete and incomplete."
  [m1 m2]
  (cond
    (complete? m1) complete
    (complete? m2) complete
    :else incomplete))

(defn add-parser-stream
  "Concats the input and more flag from two different parsers and calls
  the function f with the result."
  [i0 m0 i1 m1 f]
  (f (concat i0 i1) (concat-more m0 m1)))

(defn fail-parser
  "Parser that will always fail, you may provide an error message msg that
  will be shown on the final result."
  [msg]
  (fn failed-parser [i0 m0 ff _sf]
    (ff i0 m0 [] (str "Failed reading: " msg))))

(defn always [a]
  (fn new-parser [i0 m0 ff sf] (sf i0 m0 a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser Monad implementation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmonad parser-m
  [ m-result (fn result-fn [a]
               (always a))

    m-bind   (fn bind-fn [p f]
               (fn parser-continuation [i0 m0 ff sf]
                 (letfn [(new-sf [i1 m1 a] ((f a) i1 m1 ff sf))]
                 (p i0 m0 ff new-sf))))

    m-zero   (fail-parser "m-zero")

    m-plus   (fn m-plus-fn [p1 p2]
               (fn m-plus-parser [i0 m0 ff sf]
                 (letfn [
                   (new-ff [i1 m1 _ _]
                     (p2 i1 m1 ff sf))]
                 (p1 i0 m0 new-ff sf))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser building macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parser
  "Allows the use of monadic functions with the parser-m monad binded
  to m-bind and m-result."
  [& forms]
  `(with-monad parser-m ~@forms))

(defmacro do-parser
  "Allows the use of monadic do statements with the parser-m monad binded
  to m-bind and m-result."
  [steps result]
  `(domonad parser-m ~steps ~result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Continuation Results
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- failure-fn [i0 _m0 stack msg]
  (ResultFailure. i0 stack msg))

(defn- success-fn [i0 _m0 result]
  (ResultDone. i0 result))

(defn prompt
  "This is used for continuations of parsers (when there is not
  enough input available for the parser to either succeed or fail)."
  [i0 _m0 ff sf]
  (fn [s]
    (if (empty? s)
      (ff i0 complete)
      (sf (concat i0 (seq s)) incomplete))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parsing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse
  "Parses the given input with a parser, this may return a result that
  could either be a success, a failure , or a continuation that
  will require more input in other to finish. The parser continuation
  will halt as soon as an empty seq is given."
  [parser input]
  (parser (seq input) incomplete failure-fn success-fn))

(defn parse-once
  "Parses the given input with a parser, this may return a result that
  could either be a success or failure result (All input must be available
  at once)."
  [parser input]
  (let [result (parse parser input)]
    (if (partial? result)
      (result "")
      result)))

