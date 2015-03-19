(ns zetta.core
  (:require
   [monads.core :refer [defmonad mdo return run-mdo run-monad >>= >>]]
   [monads.util :refer [sequence-m]]
   [monads.types :as types]))

(declare parser-m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser result types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ResultDone [remainder result])
(defrecord ResultFailure [remainder stack msg])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser result query functions
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
;; ## Parser more-input constants
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This contants will serve as flags, and they will tell us if there is
;; more possible input available to the parser, when parsers require more
;; input and the `more` parameter has a value of complete, then the parser will
;; fail, otherwise if the `more` parameter has a value of incomplete, a
;; continuation function will be returned to continue the parsing.

(def
  ^{:doc "Constant value that indicates if the stream
         of the parser is complete (no more input)."}
  complete ::complete)

(def
  ^{:doc "Constant value that indicates if the stream
         of the parser is incomplete (more input to come)."}
  incomplete ::incomplete)

(defn complete?
  "Test More flag is equal to the complete constant or not."
  [m] (= m complete))

(defn incomplete?
  "Test More flag is equal to the incomplete constant or not."
  [m] (= m incomplete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- p-trampoline
  "Exact copy of the `clojure.core/trampoline` function, however it checks if
  the returned function has a :stop meta to return. This is used by the prompt
  and parse functions."
  ([f]
     (let [ret (f)]
       (if (and (fn? ret)
                (-> ret meta :stop not))
         (recur ret)
         ret)))
  ([f & args]
     (p-trampoline #(apply f args))))

(defn- concat-more
  "Merges two More flag values ('complete' and 'incomplete') and returns a
  new flag value."
  [m1 m2]
  (cond
    (complete? m1) complete
    (complete? m2) complete
    :else incomplete))

(defn add-parser-stream
  "Concats the input and `more` flag from two different parsers and calls
  the function f with the result."
  [input0 more0 input1 more1 f]
  (f (concat input0 input1) (concat-more more0 more1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser building macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parser
  "Allows the use of monadic functions bind and return which are
  binded to the parser-m monad."
  [& forms]
  `(run-monad parser-m ~@forms))

(defmacro do-parser
  "Allows the use of 'run-mdo' statements with the bind and return
  functions binded to the parser-m monad."
  [& body]
  `(run-mdo parser-m ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Basic parsers primitives
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ### Parsers will be functions that receive 4 parameters:
;;
;; * `input`: The stream of characters (or items) we are actually parsing.
;; * `more` flag: A flag that indicates if there is more input coming up or
;;   not.
;; * `ok-fn`: A continuation function that will be executed when parsing
;;   has been executed without any failures.
;; * `err-fn`: A continuation function that will be executed when an error
;;   had occured when parsing.

(defn fail-parser
  "Parser that will always fail, you may provide an error message msg that
  will be shown on the final result."
  [msg]
  (fn failed-parser [input0 more0 err-fn _ok-fn]
    (let [err-msg (str "failed reading: " msg)]
      #(err-fn input0 more0 [err-msg] err-msg))))

(defn always
  "Returns a parser that will always succeed, this parser will return the
  parameter given."
  [a]
  (fn new-parser [input0 more0 err-fn ok-fn]
    #(ok-fn input0 more0 a)))

(defn bind-parsers
  "Receives a parser and a continuation function, the result of the parser is
  going to be given as a parameter to the f function, and this function should
  return a new parser.

  Example:

    ; Everytime we parse an 'a' character and return a \"hello\" string
    (bind-parsers (char \\a) (fn [achr] (always \"hello\")))
  "
  [p f]
  (fn parser-continuation [input0 more0 err-fn ok-fn0]
    (letfn [
      (ok-fn [input1 more1 a] ((f a) input1 more1 err-fn ok-fn0))]
      (p input0 more0 err-fn ok-fn))))

(defn join-parsers
  "Merges two parsers together and returns a new parser that will execute
  parser p1, in case this fails, it is going to execute parser p2.

  Example:

    ;; Parses either the character a or the character b
    (join-parsers (char \\a) (char \\b))
  "
  [p1 p2]
  (fn m-plus-parser [input0 more0 err-fn0 ok-fn]
    (letfn [
            (err-fn [input1 more1 _ _]
              ((with-parser p2) input1 more1 err-fn0 ok-fn))]
      ((with-parser p1) input0 more0 err-fn ok-fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Parser Monad implementation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmonad parser-m
  (mreturn [_ v] (always v))
  (bind [m mv f]
        (bind-parsers mv #(run-monad m (f %))))

  types/MonadFail
  (fail [m err] (fail-parser err))

  types/MonadPlus
  (mzero [m] (fail-parser "mzero"))
  (mplus [_ lr]
         (join-parsers (first lr) (second lr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Continuation Results
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- failure-fn
  "The initial `err-fn` for all parsers that are executed on the zetta-parser
  library."
  [input0 _more0 stack msg]
  (ResultFailure. input0 stack msg))

(defn- success-fn
  "The initial `ok-fn` for all parsers that are executed on the zetta-parser
  library."
  [input0 _more0 result]
  (ResultDone. input0 result))

(defn prompt
  "This is parser is used to return continuations (when there is not
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
;; ## Parsing functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse
  "Uses the given parser to process the input, this function may return a
  result that could either be a success, a failure, or a continuation that
  will require more input in other to finish. The parser continuation
  will halt as soon as an empty seq is given."
  [parser input]
  (p-trampoline parser (seq input) incomplete failure-fn success-fn))

(defn parse-once
  "Uses the given parser to process the input, this may return a result that
  could either be a success or failure result (All input must be available
  at once when using this function)."
  [parser input]
  (let [result (parse parser input)]
    (if (partial? result)
      (result "")
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ## Monad & Applicative utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ### Haskell's applicative operators

(defn- bind-ignore-step
  "Internal function used by the `>>` macro."
  [mresult p1]
  `(>>= ~mresult (fn [~'_]
   ~p1)))

(defmacro *>
  "Composes two or more parsers returning the result of the rightmost one."
  [& more]
  `(with-parser
    ~(reduce bind-ignore-step more)))

(defmacro <*
  "Composes two or more parsers, returning the result of the leftmost one."
  [& more]
  (let [step (first more)
        steps (rest more)]
    `(with-parser
       (>>= ~step (fn [~'result#]
                    (>> ~(reduce >> steps)
                        (return ~'result#)))))))

(defn <$>
  "Maps the function f to the results of the given parsers, applicative
  functor result is going to be a parameter for function f.

  Example:

    (<$> + number number)

  Where `number` is a parser that will return a number from the parsed input."
  [f & more]
  (do-parser
   params <- (sequence-m more)
   (return (apply f params))))

(def <|>
  "Alias for join-parsers function."
  join-parsers)
