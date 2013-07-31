(defproject org.van-clj/zetta-parser "0.0.5-SNAPSHOT"
  :description "Powerful monadic parser combinator in Clojure (Haskell attoparsec's port)"
  :author "Roman Gonzalez"
  :warn-on-reflection true
  :repositories { "sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.clojure/monads "1.0.3-SNAPSHOT"]
                 [org.clojure/algo.monads "0.1.4"]
                 ]
  :dev-dependencies [[lein-autodoc "0.9.0"]
                     [marginalia "0.7.0-SNAPSHOT"]
                     [lein-marginalia "0.7.0-SNAPSHOT"]])
