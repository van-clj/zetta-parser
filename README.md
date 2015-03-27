# zetta-parser [![Build Status](https://secure.travis-ci.org/van-clj/zetta-parser.png)](http://travis-ci.org/van-clj/zetta-parser)

zetta-parser provides an easy to use Parser combinator library that allows
you to parse strings easily by composing simple parsers together to create
more powerful ones.

Basic parsers can be found in `zetta.parser.seq`, this parsers will work
with mostly any type of items you find on a stream, some others such
as `string` and `number` expect to process a stream of characters.

## Install

```clojure
[org.van-clj/zetta-parser "0.1.0"]
```

## Usage

zetta-parser provides several namespaces, each with an specific functionality:

* `zetta.core`
  Holds the basic functions to start using a zetta-parser, such
  as the parser runners, a monadic implementation for parsers, etc.

* `zetta.parser.seq`
  Holds the most basic parsers you may find, you can use this parsers
  out of the box to create more complex ones using the `zetta.combinators`
  namespace.

* `zetta.parse.string`
  Implements some of the `zetta.parser.seq` namespace parsers so that they
  always return a string result rather than a seq

* `zetta.combinators`
  Contains useful parsers transformers like many, sep-by, among others, this
  functions will allow you to enhance the behavior of simple parsers to allow
  them parse more complex inputs.

### do-parse Notation

do-parse is a macro that will allow you to implement parsers using a monadic
notation like the one provided by [bwo monads
library](https://github.com/bwo/monads). This kinds of parsers are really handy
when the behavior of the parser changes as you parse through the input.

```clojure
(ns example
  (:require
  [zetta.core :refer :all]
  [zetta.parser.seq :as pseq]
  [zetta.parser.string :as pstr]
  [zetta.combinators :as pc]))

;; sub parsers that are going to be used
(def parse-movie ...)
(def parse-patient ...)
(def parse-program ...)

(def parse-professional
  (do-parser
    pseq/skip-spaces

    name <- (pstr/take-till #(Character/isSpace %))
    ;; ^ assigns name to a string up until a space

    pseq/skip-spaces

    profession <- (pstr/take-till #(Character/isSpace %))
    ;; ^ assigns profession to a string up until a space

    ;; check for profession to change parse strategies
    ;; dynamically
    (cond
      (= profession "actor")
      (do-parser
       movies <- (pc/sep-by1 parse-movie (pseq/char \,))
       ;; ^ parse many movies separated by commas, using
       ;; the parser of a single movie
       (always (Actor. name movies)))

      (= profession "doctor")
      (do-parser
       patients <- (pc/sep-by1 parse-patient (pseq/char \,))
       ;; ^ parse many patients separated by commas, using
       ;; the parser of a single patient
       (always (Doctor. name patients)))

      (= profession "programmer")
      (do-parser
       programs <- (pc/sep-by1 parse-program (pseq/char \,))
       ;; ^ parse many programs separated by commas, using
       ;; the parser of a single program
       (always (Programmer. name programs)))

      :else
      (fail-parser (str "Invalid profession: " profession))
      ;; ^ fail parser if an invalid profession is given
      )))
```

### Applicative Functors

Most of the times however, the behavior of your parser won't change
depending on the input you are parsing, this is when the `with-parser` macro
and the applicative functors macros come handy; zetta-parser provides
high order macros to go through the input and return the types you want.

The `<$>` function will receive a normal function as it's first parameter,
the rest of the parameters are going to be a parsers, at the end the result
of each parser is going to be an input parameter for the function that was
specified in the first parameter of the function.

The `*>` macro will receive multiple parsers, is going to execute each of
them, and is going to return the value of the last parser to the right, there
is also the `<*` macro that will do the same thing, but will return the value
of first parser to the left.

Example:

```clojure
(def parse-programmer
  (<$> #(Programmer. %1 %2)
       ; ^ A function that is going to receive two parameters, two
       ;   parsers should follow this parameter.

       (*> spaces (many-till space)) ; this is %1
       ; ^ this parser will parse spaces, ignore them and return the result
       ;   of the (many-till space) parser, this will be the %1 on the
       ;   function given on the first parameter.

       (*> spaces (many-till space))))
       ; ^ this will do the same as the parser given in the second parameter
       ;   of <$>, the return value of this will be %2 on the the function
       ;   given on the first parameter.

```

### parse & parse-once

Most of the parser libraries out there will parse as long as you have all
the input you want to parse _at once_, this is really limiting given that
sometimes all the input to parse is not available (input streaming from
a connection and such).

zetta-parser provides the `parse` function which will parse the given input
using a given parser, if there is not enough input to either fail or return
a result, the parse function will return a continuation function that will
receive the remaining of the input when available, if this continuation
function receives a string, this function will either return a parsed result,
a failure or another function continuation. In case you have a continuation
and you pass an empty string to it, the parser will stop and will return either
a failure or a successful parsed result.

The `parse-once` function will behave like the parse function of any of the
other parser libraries.

## I want more info!

For more info, please clone the project and execute `lein marg` to get a great
summary of the `zetta.parser.seq` and `zetta.combinators` namespaces.

## Full Examples

### Parsing a CSV file

```clojure
(ns zetta.examples.csv
  ^{ :doc "Naive CSV parser" }
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [zetta.core :refer :all]
            [zetta.combinators
             :refer [sep-by1 around many many1 choice]]
            [zetta.parser.seq
             :refer [char not-char spaces eol end-of-input]]))

(defrecord CSVFile [titles values])

(def csv-sep
  (char \,))

(def csv-key
  (<$> str/join
       (many1
         (around spaces (not-char #{\, \newline})))))

(def csv-entry
  (<* (sep-by1 csv-key
               csv-sep)
       (<|> eol end-of-input)))

(def csv-file
  (<$> #(CSVFile. %1 %2)
       csv-entry
       (many csv-entry)))
```

## License

Copyright (C) 2012-2015 Roman Gonzalez and contributors.

Distributed under the Eclipse Public License, the same as Clojure.
