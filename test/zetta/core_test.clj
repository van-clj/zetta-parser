(ns zetta.core-test
  (:require
   [clojure.test :refer :all]
   [zetta.core :refer :all]
   [zetta.parser.seq :as p]))

(deftest fmap-applicative-functor-test
  (let [a-monad (<$> str (<* p/digit p/space)
                         (<* p/digit p/space)
                         p/end-of-input)
        result  (parse-once a-monad "4 4 ")]
    (is (= "44" (:result result)))))
