(ns zetta.tests.parser.core-test
  ^:cljs-macro
  (:require
   [^{:cljs buster-cljs.macros}
    buster-cljs.clojure :refer [deftest it is]]
   [zetta.parser.macros :refer [<*]])
  (:require
   [zetta.parser.core :refer [<$> parse-once]]
   [zetta.parser.seq :as p]))

(deftest fmap-applicative-functor-test
  (it "works correctly"
    (let [a-parser (<$> str
                        (<* p/digit p/space)
                        (<* p/digit p/space)
                        p/end-of-input)
          result (parse-once a-parser "4 4 ")]
      (is (= "44" (:result result))))))
