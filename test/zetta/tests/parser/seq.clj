(ns zetta.tests.parser.seq
  (:use clojure.test)

  (:use zetta.core)
  (:require [zetta.parser.seq :as p]))


(deftest satisfy?-test
  (let [result (p/parse-once (p/satisfy? #(= 0 (mod % 2)))
                             [10])]
    (is (done? result))
    (is (= 10 (:result result)))))

(deftest satisfy?-no-initial-match-test
  (let [result (p/parse-once (p/satisfy? #(= 0 (mod % 2)))
                              [5])]
    (is (failure? result))))

(deftest skip-test
  (let [result (p/parse-once (p/skip #(Character/isDigit %))
                             "432")]
    (is (nil? (:result result)))
    (is (= [\3 \2] (:remainder result)))))

(deftest skip-no-initial-match-test
  (let [result (p/parse-once (p/skip #(Character/isDigit %))
                             "hello")]
    (is (failure? result))))

(deftest take-with-test
  (let [result (p/parse-once
                 (p/take-with 4 (partial every? #(Character/isDigit %)))
                 "12345")]
  (is (= [\1 \2 \3 \4] (:result result)))))

(deftest take-with-no-initial-match-test
  (let [result (p/parse-once
                 (p/take-with 4 (partial every? #(Character/isDigit %)))
                 "12ab3")]
  (is (failure? result))))


(deftest take-test
  (let [result (p/parse-once
                 (p/take 5)
                 "12ab hell")]
    (is (= [\1 \2 \a \b \space] (:result result)))))

(deftest take-no-initial-match-test
  (let [result (p/parse-once
                 (p/take 5)
                 "12ab")]
    (is (failure? result))))


(deftest string-test
  (let [result (p/parse-once
                 (p/string "hello")
                 "hello world")]
    (is (= "hello" (:result result)))
    (is (= (seq " world") (:remainder result)))))


(deftest string-no-initial-match-test
  (let [result (p/parse-once
                 (p/string "hello")
                 "other world")]
    (is (failure? result))
    (is (= (seq "other world") (:remainder result)))))

(deftest skip-while-test
  (let [result (p/parse-once
                 (p/skip-while #(Character/isWhitespace %))
                 "    \tanother test")]
    (is (done? result))
    (is (= (seq "another test") (:remainder result)))))

(deftest skip-while-no-initial-match-test
  (let [result (p/parse-once
                 (p/skip-while #(Character/isWhitespace %))
                 "another test")]
    (is (done? result))
    (is (= (seq "another test") (:remainder result)))))

(deftest take-while-test
  (let [result (p/parse-once
                 (p/take-while #(Character/isLetter %))
                 "this is just a test")]
    (is (= (seq "this") (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-while-no-initial-match-test
  (let [result (p/parse-once
                 (p/take-while #(Character/isLetter %))
                 " this is just a test")]
    (is (done? result))
    (is (= [] (:result result)))
    (is (= (seq " this is just a test") (:remainder result)))))

(deftest take-till-test
  (let [result (p/parse-once
                 (p/take-till #(= % \space))
                 "this is just a test")]
    (is (done? result))
    (is (= (seq "this") (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-till-no-initial-match-test
  (let [result (p/parse-once
                 (p/take-till #(= % \space))
                 " this is just a test")]
    (is (done? result))
    (is (= [] (:result result)))
    (is (= (seq " this is just a test") (:remainder result)))))

(deftest take-rest-test
  (let [result (p/parse-once p/take-rest "hello world")]
    (is (done? result))
    (is (= [(seq "hello world")] (:result result)))))

(deftest take-while1-test
  (let [result (p/parse-once
                 (p/take-while1 #(Character/isLetter %))
                 "this is just a test")]
    (is (= (seq "this") (:result result)))
    (is (= (seq " is just a test") (:remainder result)))))

(deftest take-while1-no-initial-match-test
  (let [result (p/parse-once
                 (p/take-while1 #(Character/isLetter %))
                 " this is just a test")]
    (is (failure? result))))

(deftest any-token-test
  (let [result (p/parse-once p/any-token "1bc")]
    (is (done? result))
    (is (= \1 (:result result)))))

(deftest char-test
  (let [result (p/parse-once (p/char \a) "abc")]
    (is (done? result))
    (is (= \a (:result result)))))

(deftest char-no-initial-match-test
  (let [result (p/parse-once (p/char \a) "1bc")]
    (is (failure? result))
    (is (= (seq "1bc") (:remainder result)))))

(deftest number-test
  (let [result (p/parse-once p/number "123")]
    (is (done? result))
    (is (= 123 (:result result)))))

(deftest number-no-initial-match-test
  (let [result (p/parse-once p/number "john doe")]
    (is (failure? result))))

(deftest end-of-input-test
  (let [result (p/parse-once p/end-of-input "")]
    (is (done? result))))

(deftest end-of-input-no-initial-match-test
  (let [result (p/parse-once p/end-of-input "hello")]
    (is (failure? result))))

(deftest at-end?-test
  (let [result1 (p/parse-once p/at-end? "")
        result2 (p/parse-once p/at-end? "hello")]
    (is (:result result1))
    (is (not (:result result2)))))

(deftest eol-test
  (let [result1 (p/parse-once p/eol "\n")
        result2 (p/parse-once p/eol "\r\n")]
    (is (done? result1))
    (is (done? result2))))

(deftest eol-no-initial-match-test
  (let [result (p/parse-once p/eol "other")]
    (is (failure? result))))

