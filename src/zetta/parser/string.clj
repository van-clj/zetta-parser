(ns zetta.parser.string
  (:refer-clojure :exclude [take take-while])
  (:require [clojure.core :as core])

  (:use     [zetta.core])
  (:require [zetta.parser.seq :as pseq]))

(def to-str (partial apply str))

(defn take-with [n pred]
  (with-parser
    (<$> to-str
         (pseq/take-with n pred))))

(defn take [n]
  (with-parser
    (take-with n (constantly true))))

(defn string [s]
  (with-parser
    (take-with (count s) #(= s %))))

(defn take-while [pred]
  (with-parser
    (<$> to-str
         (pseq/take-while pred))))

(defn take-till [pred]
  (with-parser
    (take-while (complement pred))))

(def take-rest
  (with-parser
    (<$> #(map to-str %) pseq/take-rest)))

(defn take-while1 [pred]
  (with-parser
    (<$> to-str (pseq/take-while1 pred))))


