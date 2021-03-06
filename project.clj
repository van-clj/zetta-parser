(defproject org.van-clj/zetta-parser "0.1.0"
  :description "Powerful monadic parser combinator in Clojure (Haskell attoparsec's port)"
  :author "Roman Gonzalez"
  :repositories { "sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [bwo/monads "0.2.2"]]
  :plugins [[lein-marginalia "0.8.0"]])
