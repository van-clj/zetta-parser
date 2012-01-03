(ns zetta.tests.core
  (:use clojure.test)
  (:use [clojure.algo.monads
         :only
         [with-monad]])

  (:use zetta.core)
  (:require [zetta.seq :as p]))

(deftest fmap-applicative-functor-test
  (let [a-monad (with-monad parser-m
                  (<$> str (<* p/digit p/space)
                           (<* p/digit p/space)
                           p/end-of-input))
        result  (p/parse-once a-monad "4 4 ")]
    (is (= "44" (:result result)))))

