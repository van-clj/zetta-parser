(ns zetta.core
  (:use [clojure.algo.monads
         :only
         [defmonad defmonadfn domonad with-monad m-seq]]))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad & Applicative utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Haskell like operators

(defmonadfn >>= [m1 f]
  (m-bind m1 f))

(defn- bind-ignore-step [mresult m1]
  `(>>= ~mresult (fn [~'_]
   ~m1)))

(defmacro >> [& more]
  (reduce bind-ignore-step more))

;; Haskell Applicative operators

(defmacro *> [& more]
  `(>> ~@more))

(defmacro <* [& more]
  (let [step (first more)
        steps (rest more)]
  `(>>= ~step (fn [~'result#]
   (>> ~(reduce bind-ignore-step steps)
        (~'m-result ~'result#))))))

(defmonadfn <$> [f & more]
  (>>= (m-seq more) (fn [params]
  (m-result (apply f params)))))

(defmonadfn <|> [m1 m2]
  (m-plus m1 m2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser Monad utility functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn concat-more [m1 m2]
  (cond
    (complete? m1) complete
    (complete? m2) complete
    :else incomplete))

(defn add-parser-stream [i0 m0 i1 m1 f]
  (f (concat i0 i1) (concat-more m0 m1)))

(defn fail-parser [msg]
  (fn failed-parser [i0 m0 ff _sf]
    (ff i0 m0 [] (str "Failed reading: " msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser Monad implementation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 (p1 i0 m0 new-ff sf))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parser building macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-parser [& forms]
  `(with-monad parser-m ~@forms))

(defmacro do-parser [steps result]
  `(domonad parser-m ~steps ~result))
