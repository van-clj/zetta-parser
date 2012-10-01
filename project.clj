(defproject org.van-clj/zetta-parser "0.0.5-SNAPSHOT"
  :description "Powerful monadic parser combinator in Clojure (Haskell attoparsec's port)"
  :author "Roman Gonzalez"
  :warn-on-reflection true
  :repositories { "sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [psy.ai/algo.monads "0.1.3-SNAPSHOT"]]
  :dev-dependencies [[lein-autodoc "0.9.0"]
                     [marginalia "0.7.0-SNAPSHOT"]
                     [lein-marginalia "0.7.0-SNAPSHOT"]])
