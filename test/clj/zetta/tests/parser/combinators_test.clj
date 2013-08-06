(ns zetta.tests.parser.combinators-test
  ^:cljs-macro
  (:require
   ^{:cljs [buster-cljs.macros :refer [initialize-buster deftest it is]]}
   [buster-cljs.clojure :refer [deftest it is]])
  (:require
   [zetta.parser.core :as z]
   [zetta.parser.seq :as p]
   [zetta.parser.combinators :as c]))

#_(:cljs (initialize-buster))

(deftest combinators

  (it "many"
    (let [result (z/parse-once (c/many p/digit)
                             "12345")]
      (is (= [\1 \2 \3 \4 \5] (:result result)))))

  (it "many no inital match"
    (let [result (z/parse-once (c/many p/digit)
                             "")]
      (is (not (z/failure? result)))))

  (it "many1"
    (let [result (z/parse-once (c/many1 p/digit)
                             "12345")]
      (is (= [\1 \2 \3 \4 \5] (:result result)))))

  (it "many1 no inital match"
    (let [result (z/parse-once (c/many1 p/digit)
                             "")]
      (is (z/failure? result))))

  (it "sep-by1"
    (let [result (z/parse-once (c/sep-by1 p/digit p/space)
                             "5 4 3 2")]
      (is (= [\5 \4 \3 \2] (:result result)))))

  (it "choice"
    (let [result (z/parse-once
                  (c/many (c/choice [p/digit p/letter]))
                  "a43bc2f")]
      (is (z/done? result))
      (is (= [\a \4 \3 \b \c \2 \f] (:result result)))))

  (it "choice no initial match"
    (let [result (z/parse-once
                  (c/choice [p/digit p/letter])
                  "@&3bc2f")]
      (is (z/failure? result))))

  (it "replicate"
    (let [result (z/parse-once
                  (c/replicate 5 p/digit)
                  "123456")]
      (is (z/done? result))
      (is (= [\1 \2 \3 \4 \5] (:result result))
          (is (= [\6] (:remainder result))))))

  (it "replicate no initial match"
    (let [result (z/parse-once
                  (c/replicate 5 p/digit)
                  "1234abc")]
      (is (z/failure? result))
      (is (= [\a \b \c] (:remainder result)))))

  (it "option"
    (let [result (z/parse-once
                  (c/option \y p/letter)
                  "a")]
      (is (= \a (:result result)))))

  (it "option no initial match"
    (let [result (z/parse-once
                  (c/option \y p/letter)
                  "1")]
      (is (z/done? result))
      (is (= \y (:result result)))))


  (it "sep-by1 no initial match"
    (let [result (z/parse-once (c/sep-by1 p/digit p/space)
                             "")]
      (is (z/failure? result))))

  (it "sep-by"
    (let [result (z/parse-once (c/sep-by p/digit p/space)
                             "5 4 3 2")]
      (is (= [\5 \4 \3 \2] (:result result)))))

  (it "sep-by no initial match test"
    (let [result (z/parse-once (c/sep-by p/digit p/space)
                             "")]
      (is (not (z/failure? result)))
      (is (= [] (:result result)))))

  (it "many-till"
    (let [result (z/parse-once
                  (c/many-till p/letter (p/char \@))
                  "hello@domain.com")]
      (is (= [\h \e \l \l \o] (:result result)))))

  (it "many-till no initial match"
    (let [result (z/parse-once
                  (c/many-till p/letter (p/char \@))
                  "@domain.com")]
      (is (z/done? result))
      (is (= [] (:result result)))))

  (it "skip-many"
    (let [result (z/parse-once
                  (c/skip-many p/letter)
                  "abc123")]
      (is (z/done? result))
      (is (= [\1 \2 \3] (:remainder result)))))

  (it "skip-many no initial match"
    (let [result (z/parse-once
                  (c/skip-many p/letter)
                  "123")]
      (is (z/done? result))
      (is (= [\1 \2 \3] (:remainder result)))))

  (it "skip-many1"
    (let [result (z/parse-once
                  (c/skip-many1 p/letter)
                  "abc123")]
      (is (z/done? result))
      (is (= [\1 \2 \3] (:remainder result)))))

  (it "skip-many1 no initial match"
    (let [result (z/parse-once
                  (c/skip-many1 p/letter)
                  "123")]
      (is (z/failure? result)))))
