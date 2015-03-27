(ns zetta.examples.clojure
  ^{:doc "A parser for a subset of Clojure"}
  (:refer-clojure :exclude [char])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [zetta.core :as z
             :refer [<$> <* *>]]
            [zetta.combinators
             :refer [sep-by around many many1 choice]]
            [zetta.parser.seq
             :refer [digit string char not-char whitespace]]))

(def whitespaces
  (many whitespace))

(def clojure
  (around
   whitespaces
   (choice
    [(<$> (comp #(Integer/parseInt %) str/join)
          (many1 digit))
     (<$> (comp symbol str/join)
          (many1 (not-char #{\ \newline \( \) \[ \] \# \{ \}})))
     (<$> set
          (*> (string "#{")
              (<* (sep-by clojure whitespaces) (char \}))))
     (<$> (partial apply vector)
          (*> (char \[)
              (<* (sep-by clojure whitespaces) (char \]))))
     (*> (char \()
         (<* (sep-by clojure whitespaces) (char \))))])))

(defn -main []
  (let [x (z/parse-once clojure "(defn f [x]\n  [x 'a #{1 1 2} 123])")]
    (prn (:result x))
    (prn (:remainder x))))
