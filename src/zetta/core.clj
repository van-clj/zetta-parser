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
  [input0 more0 input1 more1 f]
  (f (concat input0 input1) (concat-more more0 more1)))

(defn fail-parser
  "Parser that will always fail, you may provide an error message msg that
  will be shown on the final result."
  [msg]
  (fn failed-parser [input0 more0 err-fn _ok-fn]
    #(err-fn input0 more0 [] (str "Failed reading: " msg))))

(defn- p-trampoline
  "Exact copy of the `clojure.core/trampoline` function,
  however it checks if the returned function has a :stop
  meta to return. This is used by the prompt and
  the parse functions."
  ([f]
     (let [ret (f)]
       ;(println ret)
       (if (and (fn? ret)
                (-> ret meta :stop not))
         (recur ret)
         ret)))
  ([f & args]
     (p-trampoline #(apply f args))))

(defn always [a]
  (fn new-parser [input0 more0 err-fn ok-fn]
    #(ok-fn input0 more0 a)))

(defn bind-parsers [p f]
  (fn parser-continuation [input0 more0 err-fn ok-fn0]
    (letfn [
      (ok-fn [input1 more1 a] ((f a) input1 more1 err-fn ok-fn0))]
      (p input0 more0 err-fn ok-fn))))

(defn join-parsers [p1 p2]
  (fn m-plus-parser [input0 more0 err-fn0 ok-fn]
                 (letfn [
                   (err-fn [input1 more1 _ _]
                     (p2 input1 more1 err-fn0 ok-fn))]
                 (p1 input0 more0 err-fn ok-fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser Monad implementation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmonad parser-m
  [ m-result (fn result-fn [a]
               (always a))

    m-bind   (fn bind-fn [p f]
               (bind-parsers p f))

    m-zero   (fail-parser "m-zero")

    m-plus   (fn m-plus-fn [p1 p2]
               (join-parsers p1 p2))])

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

(defn- failure-fn [input0 _more0 stack msg]
  (ResultFailure. input0 stack msg))

(defn- success-fn [input0 _more0 result]
  (ResultDone. input0 result))

(defn prompt
  "This is used for continuations of parsers (when there is not
  enough input available for the parser to either succeed or fail)."
  [input0 _more0 err-fn ok-fn]
  (with-meta
    (fn [new-input]
      (if (empty? new-input)
        (p-trampoline err-fn input0 complete)
        (p-trampoline ok-fn (concat input0 (seq new-input)) incomplete)))
    {:stop true}))

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
  (p-trampoline parser (seq input) incomplete failure-fn success-fn))

(defn parse-once
  "Parses the given input with a parser, this may return a result that
  could either be a success or failure result (All input must be available
  at once)."
  [parser input]
  (let [result (parse parser input)]
    (if (partial? result)
      (result "")
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad & Applicative utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Haskell like operators

(defn >>=
  "Equivalent to the m-bind function for monads."
  [p f]
  (bind-parsers p f))

(defn- bind-ignore-step [mresult p1]
  `(>>= ~mresult (fn [~'_]
   ~p1)))

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
        (always ~'result#))))))

(defn <$>
  "Maps the function f to the results of the given applicative functors,
  each applicative functor result value is going to be a parameter for
  function f.

  Example:
    (<$> + number number)

  Where number is parser that will return a number from the input stream."
  [f & more]
  (with-parser
    (m-bind (m-seq more) (fn [params]
    (m-result (apply f params))))))

(def <|>
  "Equivalent to the m-plus function for monads."
  join-parsers)
