(ns zetta.tests.seq
  (:use clojure.test)
  (:use zetta.core)
  (:require [zetta.seq :as p]))


(deftest satisfy?-test
  (let [result (p/parse-once (p/satisfy? #(= 0 (mod % 2)))
                             [10])]
    (is (done? result))
    (is (= 10 (:result result)))))

(deftest satisfy?-no-initial-match-test
  (let [result (p/parse-once (p/satisfy? #(= 0 (mod % 2)))
                              [5])]
    (is (failure? result))))

(deftest number-test
  (let [result (p/parse-once p/number "123")]
    (is (done? result))
    (is (= 123 (:result result)))))
