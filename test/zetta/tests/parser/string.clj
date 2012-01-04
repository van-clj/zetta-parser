(ns zetta.tests.parser.string
  (:use clojure.test)

  (:use zetta.core)
  (:require [zetta.parser.string :as p]))

(deftest take-with-test
  (let [result (parse-once
                 (p/take-with 4 (partial every? #(Character/isDigit %)))
                 "12345")]
  (is (= "1234" (:result result)))))

(deftest take-with-no-initial-match-test
  (let [result (parse-once
                 (p/take-with 4 (partial every? #(Character/isDigit %)))
                 "12ab3")]
  (is (failure? result))))

(deftest take-test
  (let [result (parse-once
                 (p/take 5)
                 "12ab hell")]
    (is (= "12ab " (:result result)))))

(deftest take-no-initial-match-test
  (let [result (parse-once
                 (p/take 5)
                 "12ab")]
    (is (failure? result))))

(deftest take-while-test
  (let [result (parse-once
                 (p/take-while #(Character/isLetter %))
                 "this is just a test")]
    (is (= "this" (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-while-no-initial-match-test
  (let [result (parse-once
                 (p/take-while #(Character/isLetter %))
                 " this is just a test")]
    (is (done? result))
    (is (= "" (:result result)))
    (is (= (seq " this is just a test") (:remainder result)))))

(deftest take-till-test
  (let [result (parse-once
                 (p/take-till #(= % \space))
                 "this is just a test")]
    (is (done? result))
    (is (= "this" (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-till-no-initial-match-test
  (let [result (parse-once
                 (p/take-till #(= % \space))
                 " this is just a test")]
    (is (done? result))
    (is (= "" (:result result)))
    (is (= (seq " this is just a test") (:remainder result)))))

(deftest take-rest-test
  (let [result (parse-once p/take-rest "hello world")]
    (is (done? result))
    (is (= ["hello world"] (:result result)))))

(deftest take-while1-test
  (let [result (parse-once
                 (p/take-while1 #(Character/isLetter %))
                 "this is just a test")]
    (is (= "this" (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-while1-no-initial-match-test
  (let [result (parse-once
                 (p/take-while1 #(Character/isLetter %))
                 " this is just a test")]
    (is (failure? result))))


