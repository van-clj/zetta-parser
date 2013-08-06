;; This file was generated with lein-dalap from
;;
;; test/clj/zetta/tests/parser/core_test.clj @ Mon Aug 05 23:13:58 PDT 2013
;;
(ns zetta.tests.parser.core-test (:require-macros [buster-cljs.macros :refer [deftest it is]] [zetta.parser.macros :refer [<*]]) (:require [zetta.parser.core :refer [<$> parse-once]] [zetta.parser.seq :as p]))
(deftest fmap-applicative-functor-test (it "works correctly" (let [a-monad (<$> str (<* p/digit p/space) (<* p/digit p/space) p/end-of-input) result (parse-once a-monad "4 4 ")] (is (= "44" (:result result))))))