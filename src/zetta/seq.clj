(ns zetta.seq
  (:refer-clojure :exclude [ensure get take take-while char some])
  (:require [clojure.core :as core])
  (:use [clojure.algo.monads
         :only
         [domonad with-monad]])

  (:use zetta.core)
  (:use zetta.combinators)
  (:import [zetta.core ResultFailure ResultDone]))

(defn failure-fn [i0 _m0 stack msg]
  (ResultFailure. i0 stack msg))

(defn success-fn [i0 _m0 result]
  (ResultDone. i0 result))

(defn parse [parser input]
  (parser input incomplete failure-fn success-fn))

(defn parse-once [parser input]
  (let [result (parser input incomplete failure-fn success-fn)]
    (if (partial? result) 
      (result "")
      result)))

(defn span [pred xs]
  ((core/juxt #(core/take-while pred %) #(core/drop-while pred %)) xs))

;;;;;;;;;;;;;;;;;;;;

(defn <?> [p err-msg]
  (fn [i0 m0 ff sf]
    (letfn [
      (new-ff [i0 m0 errors msg] (ff i0 m0 (conj errors err-msg) msg))]
    (p i0 m0 new-ff sf))))


(defn prompt [i0 _m0 ff sf]
  (fn [s]
    (if (empty? s)
      (ff i0 complete)
      (sf (concat i0 s) incomplete))))

(def demand-input
  (fn [i0 m0 ff sf]
    (if (complete? m0)
      (ff i0 m0 ["demand-input"] "not enough input")
      (letfn [
        (new-ff [i m]
          (ff i m ["demand-input"] "not enough input"))
        (new-sf [i m]
          (sf i m nil))]
      (prompt i0 m0 new-ff new-sf)))))

(def want-input?
  (fn [i0 m0 _ff sf]
    (cond
      (not (empty? i0)) (sf i0 m0 true)
      (= m0 complete)   (sf i0 m0 false)
      :else
        (letfn [(new-ff [i m] (sf i m false))
                (new-sf [i m] (sf i m true))]
        (prompt i0 m0 new-ff new-sf)))))

(defn ensure [n]
  (fn [i0 m0 ff sf]
    (if (>= (count i0) n)
      (sf i0 m0 i0)
      (with-monad parser-m
        ((>> demand-input (ensure n)) i0 m0 ff sf)))))

(def get
  (fn [i0 m0 _ff sf]
    (sf i0 m0 i0)))

(defn put [s]
  (fn [_i0 m0 _ff sf]
    (sf s m0 nil)))

(defn satisfy? [pred]
  (domonad parser-m
    [s     (ensure 1)
     :let  [w (first s)]
     :if (pred w)
       :then [
         _ (put (rest s))
       ]
       :else [
        _ (fail-parser "satisfy?")
       ]]
    w))

(defn skip [pred]
  (domonad parser-m
    [s (ensure 1)
     :if (pred (first s))
       :then [_ (put (rest s))]
       :else [_ (fail-parser "skip")]]
     nil))

(defn take-with [n pred]
  (domonad parser-m
    [s (ensure n)
     :let [[h t] (split-at n s)]
     :if (pred h)
       :then [_ (put t)]
       :else [_ (fail-parser "take-with")]]
     h))

(defn take [n]
  (domonad parser-m
    [result (take-with n (constantly true))] result))

(defn string [s]
  (let [vecs (vec s)]
    (domonad parser-m
      [result (take-with (count s) #(= vecs %))] result)))

(defn skip-while [pred]
  (let [go (domonad parser-m
            [t0 get
             :let [t (drop-while pred t0)]
             _ (put t)
             :if (empty? t)
             :then [
               input want-input?
               :if input
               :then [_ (skip-while pred)]
               :else []
             ]
             :else []]
             nil)]
     go))

(defn take-while [pred]
  (letfn [
    (go [acc]
      (domonad parser-m
        [t0 get
         :let [[h t] (span pred t0)]
         _ (put t)
         :if (empty? t)
           :then [
             input want-input?
             :if input
               :then [result (go (conj acc h))]
               :else [result (m-result (conj acc h))]
           ]
           :else [result (m-result (conj acc h))]]
           result))]
  (domonad parser-m
    [result (go [])]
    (->> result core/reverse (apply core/concat)))))

(defn take-till [pred]
  (domonad parser-m
    [result (take-while (complement pred))] result))

(def take-rest
  (letfn [
    (go [acc]
      (domonad parser-m
        [input want-input?
         :if input
           :then [
             s get
             _ (put [])
             result (go (conj acc s))
           ]
           :else [
             result (m-result (reverse acc))
           ]]
          result))]
  (go [])))

(defn take-while-1 [pred]
  (domonad parser-m
    [input-checker get
     :if (empty? input-checker)
       :then [_ demand-input]
       :else []
     current-input get
     :let [[h t] (span pred current-input)]
     :if (empty? h)
       :then [_ (fail-parser "take-while-1")]
       :else [_ (put t)]
     :if (empty? t)
       :then [remainder (take-while pred)
              result (m-result (concat h remainder))]
       :else [result (m-result h)]]
     result))

(def any-token
  (domonad parser-m
    [result (satisfy? (constantly true))] result))

(defn char [c]
  (domonad parser-m
    [result (<?> (satisfy? #(= % c)) (str c))] result))

(def letter
  (with-monad parser-m
    (>>= (satisfy? #(Character/isLetter %))
         m-result)))

(def digit
  (with-monad parser-m
    (>>= (satisfy? #(Character/isDigit %))
         m-result)))

(def number
  (with-monad parser-m
    (<$> (comp #(Integer/parseInt %) #(apply str %))
         (many1 (satisfy? #(Character/isDigit %))))))

(def space
  (with-monad parser-m
    (satisfy? #(= % \space))))

(defn not-char [c]
  (domonad parser-m
    [result (<?> (satisfy? #(complement (= % c)))
                           (str "not" c))]
    result))

(def end-of-input
  (fn [i0 m0 ff sf]
    (if (empty? i0)
      (if (complete? m0)
        (sf i0 m0 nil)
        (letfn [
          (new-ff [i1 m1 _ _]
            (add-parser-stream i0 m0 i1 m1
                               (fn [i2 m2] (sf i2 m2 nil))))
          (new-sf [i1 m1 _]
            (add-parser-stream i0 m0 i1 m1
                               (fn [i2 m2] (ff i2 m2 [] "end-of-input"))))]
        (demand-input i0 m0 new-ff new-sf)))
      (ff i0 m0 [] "end-of-input"))))

(def at-end?
  (domonad parser-m
    [input want-input?] (not input)))

(def eol
  (with-monad parser-m
    (<|> (>>= (char \newline) (fn [_]
              (m-result nil)))
         (>>= (string "\r\n")
              (fn [_] (m-result nil))))))


