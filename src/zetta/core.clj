(ns zetta.core
  (:use [clojure.algo.monads
         :only
         [defmonad defmonadfn]]))

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

(defmonadfn >>= [m1 f]
  (m-bind m1 f))

(defmacro >> [m1 m2]
  `(>>= ~m1 (fn [~'_] ~m2)))

(defmacro *> [m1 m2]
  `(>>= ~m1 (fn [~'_] ~m2)))

(defmonadfn <* [a1 a2]
  (>>= a1 (fn [a1v]
  (>>= a2 (fn [_]
  (m-result a1v))))))

(defmonadfn m-seq
  [steps]
  (if (empty? steps)
    (m-result [])
    (>>= (first steps) (fn [h]
    (>>= (m-seq (rest steps)) (fn [t]
    (m-result (cons h t))))))))

(defmonadfn <$> [f & more]
  (>>= (m-seq more) (fn [params]
  (m-result (apply f params)))))

(defmonadfn <|> [m1 m2]
  (m-plus m1 m2))

;;;;;;;;;;;;;;;;;;;;

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

