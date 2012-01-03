(ns zetta.combinators
  (:refer-clojure :exclude [some])
  (:use [clojure.algo.monads
         :only
         [domonad with-monad]])

  (:use zetta.core)
  (:import [zetta.core ResultDone ResultFailure]))

(declare many-v)

(defn- some-v [p]
  (do-parser
    [h p
     t (many-v p)] (cons h t)))

(defn- many-v [p]
  (with-parser
    (<|> (some-v p) (m-result []))))

(defn many [p] (many-v p))

(defn some [p] (some-v p))

(defn choice [ps]
  (with-parser
    (reduce <|> ps)))

(defn option [default-val p]
  (with-parser
    (<|> p (m-result default-val))))

(defn many1 [p]
  (with-parser
    (<$> cons p (many p))))

(defn sep-by1 [p s]
  (with-parser
    (<$> cons
         p
         (<|> (*> s (sep-by1 p s))
              (m-result [])))))

(defn sep-by [p s]
  (with-parser
    (<|>  (<$> cons
               p
               (<|> (*> s (sep-by1 p s))
                    (m-result [])))
          (m-result []))))

(defn many-till [p end]
  (with-parser
    (<|> (*> end (m-result []))
         (>>= p (fn [h]
         (>>= (many-till p end) (fn [t]
         (m-result (cons h t)))))))))
