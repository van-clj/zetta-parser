(ns zetta.tests.parser.core-test
  (:require [clojure.test :refer :all]
            [zetta.parser.core :refer :all]
            [zetta.parser.seq :as p])
  (:require [zetta.parser.macros :refer [<*]]))

(deftest fmap-applicative-functor-test
  (let [a-monad (<$> str
                     (<* p/digit p/space)
                     (<* p/digit p/space)
                     p/end-of-input)
        result  (parse-once a-monad "4 4 ")]
    (is (= "44" (:result result)))))
