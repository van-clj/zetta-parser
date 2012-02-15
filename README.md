# zetta-parser [![Build Status](https://secure.travis-ci.org/van-clj/zetta-parser.png)](http://travis-ci.org/van-clj/zetta-parser)

zetta-parser provides an easy to use Parser combinator library that allows
you to parse strings easily by composing simple parsers together to create
more powerful ones.

Basic parsers can be found in `zetta.parser.seq`, this parsers will work
with mostly any type of items you find on a stream, some others such
as `string` and `number` expect to process a stream of characters.

## Install

[org.van-clj/zetta-parser "0.0.2"]

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
notation like the one provided by clojure.algo.monads. This kinds of
parsers are really handy when the behavior of the parser changes as you parse
through the input.

```clojure
(ns example
  (:use [zetta.core :only (do-parser always)]
        [zetta.parser.seq :only (space spaces)]
        [zetta.combinators :only (many-till)]))

; sub parsers that are going to be used
(def parse-movies ...)
(def parse-patients ...)
(def parse-programs ...)

(def parse-professional
  (do-parser
    [_ spaces
     ; ^ ignore spaces

     name (many-till space)
     ; ^ parse many characters until space

     _ spaces
     ; ^ ignore spaces

     profession (many-till space)
     ; ^ parse many characters until space

     :cond [
     ; ^ depending on the profession we might return one thing
     ; or the other
       (= profession "actor")
       [
         movies parse-movies
         result (always (Actor. name movies))
       ]

       (= profession "doctor")
       [
         patients parse-patients
         result (always (Doctor. name patients))
       ]

       (= profession "programmer")
       [
         programms parse-programs
         result (always (Programmer. name programs))
       ]]]
     result))
```

For more info on how to implement parsers using monadic notations check
[clojure/algo.monads](http://github.com/clojure/algo.monads) info, version
>= 0.1.3 is required.

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

## Full Examples

### Parsing a CSV file

```clojure
(ns examples.csv
  (:require [clojure.java.io :as io])

  (:use zetta.core)
  (:require [zetta.combinators :as comb])
  (:require [zetta.parser.seq :as p]))

(defrecord CSVFile [titles, values])

(def csv-hash
  (<$> #(apply hash-map %)
       (comb/sep-by (comb/many (p/not-char ','))
                    (<|> (p/char ',')
                          p/eol))))

(def csv-file
  (<$> #(CSVFile. %1 %2)
       csv-hash
       (comb/many csv-hash)))

(defn get-csv-file [path]
  (-> path
      io/file
      io/reader
      line-seq
      concat
      (parse-once csv-file)))
```

## License

Copyright (C) 2011 Roman Gonzalez.

Distributed under the Eclipse Public License, the same as Clojure.
