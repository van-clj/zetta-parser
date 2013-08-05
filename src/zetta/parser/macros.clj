(ns zetta.parser.macros
  (:refer-clojure :exclude [do])
  (:require
   [monads.macros :as monad-macro]
   [zetta.core :refer [>>= always bind-ignore-step]]))

(defmacro do
  "Allows the use of 'domonad' statements with the m-bind and m-result
  functions binded to the parser-m monad."
  [steps result]
  `(monad-macro/do always ~steps ~result))

(defmacro >>
  "Composes two or more parsers returning the result of the rightmost one."
  [& more]
  (reduce bind-ignore-step more))

(defmacro *>
  "Composes two or more parsers, returning the result value of the rightmost
  one."
  [& more]
  `(>> ~@more))

(defmacro <*
  "Composes two or more parsers, returning the result of the leftmost one."
  [& more]
  (let [step (first more)
        steps (rest more)]
  `(>>= ~step (fn [~'result#]
   (>> ~(reduce bind-ignore-step steps)
        (always ~'result#))))))