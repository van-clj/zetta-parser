(ns attoparsec.core
  (:use [clojure.algo.monads 
         :only 
         [defmonad]]))

(defrecord ResultDone [remainder result])
(defrecord ResultFailure [remainder stack msg])

(def partial? fn?)
(defn done? [result] (= (type result) ResultDone))
(defn failure? [result] (= (type result) ResultFailure))

;; Parser completion constants

(def complete ::complete)
(def incomplete ::incomplete)

(defn complete? [m] (= m complete))
(defn incomplete? [m] (= m incomplete))

(defn concat-more-input [m1 m2]
  (cond 
    (complete? m1) complete
    (complete? m2) complete
    :else incomplete))

;; Monad implementation for the parser

(defn add-parser-stream [i0 m0 i1 m1 f]
  (f (concat i0 i1) (concat-more-input m0 m1)))

(defn fail-parser [msg]
  (fn failed-parser [i0 m0 ff _sf]
    (ff i0 m0 [] (str "Failed reading: " msg))))

(defmonad parser-m
  [ m-result (fn result-fn [a]
               (fn new-parser [i0 m0 ff sf] (sf i0 m0 a)))

    m-bind   (fn bind-fn [p f]
               (fn parser-continuation [i0 m0 ff sf]
                 (letfn [(new-sf [i1 m1 a] ((f a) i1 m1 ff sf))]
                 (p i0 m0 ff new-sf))))

    m-zero   (fail-parser "m-zero")

    m-plus   (fn m-plus-fn [p1 p2]
               (fn m-plus-parser [i0 m0 ff sf]
                 (letfn [
                   (new-ff [i1 m1 _ _] 
                     (p2 i1 m1 ff sf))]
                     ;(add-parser-stream i0 m0 i1 m1 
                     ;  (fn [i2 m2] (p2 i2 m2 ff sf))))]
                 (p1 i0 m0 new-ff sf))))])

