(ns zetta.tests.parser.combinators-test
  (:require
   [clojure.test :refer :all]
   [zetta.parser.core :refer :all]
   [zetta.parser.seq :as p]
   [zetta.parser.combinators :as c]))

(deftest many-test
  (let [result (parse-once (c/many p/digit)
                             "12345")]
  (is (= [\1 \2 \3 \4 \5] (:result result)))))

(deftest many-no-inital-match-test
  (let [result (parse-once (c/many p/digit)
                             "")]
  (is (not (failure? result)))))

(deftest many1-test
  (let [result (parse-once (c/many1 p/digit)
                             "12345")]
  (is (= [\1 \2 \3 \4 \5] (:result result)))))

(deftest many1-no-inital-match-test
  (let [result (parse-once (c/many1 p/digit)
                             "")]
  (is (failure? result))))

(deftest sep-by1-test
  (let [result (parse-once (c/sep-by1 p/digit p/space)
                             "5 4 3 2")]
  (is (= [\5 \4 \3 \2] (:result result)))))

(deftest choice-test
  (let [result (parse-once
                 (c/many (c/choice [p/digit p/letter]))
                 "a43bc2f")]
  (is (done? result))
  (is (= [\a \4 \3 \b \c \2 \f] (:result result)))))

(deftest choice-no-initial-match-test
  (let [result (parse-once
                 (c/choice [p/digit p/letter])
                 "@&3bc2f")]
  (is (failure? result))))

(deftest replicate-test
  (let [result (parse-once
                 (c/replicate 5 p/digit)
                 "123456")]
  (is (done? result))
  (is (= [\1 \2 \3 \4 \5] (:result result))
  (is (= [\6] (:remainder result))))))

(deftest replicate-no-initial-match-test
  (let [result (parse-once
                 (c/replicate 5 p/digit)
                 "1234abc")]
  (is (failure? result))
  (is (= [\a \b \c] (:remainder result)))))

(deftest option-test
  (let [result (parse-once
                 (c/option \y p/letter)
                 "a")]
  (is (= \a (:result result)))))

(deftest option-no-initial-match-test
  (let [result (parse-once
                 (c/option \y p/letter)
                 "1")]
  (is (done? result))
  (is (= \y (:result result)))))


(deftest sep-by-1-no-initial-match-test
  (let [result (parse-once (c/sep-by1 p/digit p/space)
                             "")]
  (is (failure? result))))

(deftest sep-by-test
  (let [result (parse-once (c/sep-by p/digit p/space)
                             "5 4 3 2")]
  (is (= [\5 \4 \3 \2] (:result result)))))

(deftest sep-by-test-no-initial-match-test
  (let [result (parse-once (c/sep-by p/digit p/space)
                             "")]
  (is (not (failure? result)))
  (is (= [] (:result result)))))

(deftest many-till-test
  (let [result (parse-once
                 (c/many-till p/letter (p/char \@))
                 "hello@domain.com")]
  (is (= [\h \e \l \l \o] (:result result)))))

(deftest many-till-no-initial-match-test
  (let [result (parse-once
                 (c/many-till p/letter (p/char \@))
                 "@domain.com")]
  (is (done? result))
  (is (= [] (:result result)))))

(deftest skip-many-test
  (let [result (parse-once
                (c/skip-many p/letter)
                "abc123")]
  (is (done? result))
  (is (= [\1 \2 \3] (:remainder result)))))

(deftest skip-many-no-initial-match-test
  (let [result (parse-once
                (c/skip-many p/letter)
                "123")]
  (is (done? result))
  (is (= [\1 \2 \3] (:remainder result)))))

(deftest skip-many1-test
  (let [result (parse-once
                (c/skip-many1 p/letter)
                "abc123")]
  (is (done? result))
  (is (= [\1 \2 \3] (:remainder result)))))

(deftest skip-many1-no-initial-match-test
  (let [result (parse-once
                (c/skip-many1 p/letter)
                "123")]
  (is (failure? result))))
