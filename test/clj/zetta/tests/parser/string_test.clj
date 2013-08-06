(ns zetta.tests.parser.string-test
  ^:cljs-macro
  (:require
   ^{:cljs [buster-cljs.macros :refer [initialize-buster deftest it is]]}
   [buster-cljs.clojure :refer [deftest it is]])
  (:require
   #_(:cljs [goog.string :as _gstring])
   [zetta.parser.core :refer [parse-once done? failure?]]
   [zetta.parser.string :as p]))


#_(:cljs (initialize-buster))

(deftest string-parser

  (it "take-with"
    (let [result (parse-once
                  (p/take-with 4 (partial
                                  every?
                                  ^{:cljs '#(goog.string.isNumeric %)}
                                  #(Character/isDigit ^java.lang.Character %)))
                  "12345")]
      (is (= "1234" (:result result)))))

  (it "take-with no initial match"
    (let [result (parse-once
                  (p/take-with 4 (partial
                                  every?
                                  ^{:cljs '#(goog.string.isNumeric %)}
                                  #(Character/isDigit ^java.lang.Character %)))
                  "12ab3")]
      (is (failure? result))))

  (it "take"
    (let [result (parse-once
                  (p/take 5)
                  "12ab hell")]
      (is (= "12ab " (:result result)))))

  (it "take no initial match"
    (let [result (parse-once
                  (p/take 5)
                  "12ab")]
      (is (failure? result))))

  (it "take-while"
    (let [result (parse-once
                  (p/take-while
                   ^{:cljs '#(goog.string.isAlpha %)}
                   #(Character/isLetter ^java.lang.Character %))
                  "this is just a test")]
      (is (= "this" (:result result)))
      (is (= (seq " is just a test") (:remainder result)))))

  (it "take-while no initial match"
    (let [result (parse-once
                  (p/take-while
                   ^{:cljs '#(goog.string.isAlpha %)}
                   #(Character/isLetter ^java.lang.Character %))
                  " this is just a test")]
      (is (done? result))
      (is (= "" (:result result)))
      (is (= (seq " this is just a test") (:remainder result)))))

  (it "take-till"
    (let [result (parse-once
                  (p/take-till #(= % \space))
                  "this is just a test")]
      (is (done? result))
      (is (= "this" (:result result)))
      (is (= (seq " is just a test") (:remainder result)))))

  (it "take-till no initial match"
    (let [result (parse-once
                  (p/take-till #(= % \space))
                  " this is just a test")]
      (is (done? result))
      (is (= "" (:result result)))
      (is (= (seq " this is just a test") (:remainder result)))))

  (it "take-rest"
    (let [result (parse-once p/take-rest "hello world")]
      (is (done? result))
      (is (= ["hello world"] (:result result)))))

  (it "take-while1"
    (let [result (parse-once
                  (p/take-while1
                   ^{:cljs '#(goog.string.isAlpha %)}
                   #(Character/isLetter ^java.lang.Character %))
                  "this is just a test")]
      (is (= "this" (:result result)))
      (is (= (seq " is just a test") (:remainder result)))))

  (it "take-while1 no initial match"
    (let [result (parse-once
                  (p/take-while1
                   ^{:cljs '#(goog.string.isAlpha %)}
                   #(Character/isLetter ^java.lang.Character %))
                  " this is just a test")]
      (is (failure? result)))))
