(defproject org.van-clj/zetta-parser "0.0.5-SNAPSHOT"
  :description "Powerful monadic parser combinator in Clojure (Haskell attoparsec's port)"
  :author "Roman Gonzalez"
  :warn-on-reflection true
  :repositories { "sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [net.clojure/monads "1.0.3-SNAPSHOT"]
                 [com.birdseye-sw/buster-cljs "0.1.2"]]

  :plugins [[lein-cljsbuild "0.3.2"]
            [com.birdseye-sw/lein-dalap "0.1.2-SNAPSHOT"]]

  :hooks [leiningen.dalap]

  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["test/clj"]

  ;; :dev-dependencies [[lein-autodoc "0.9.0"]
  ;;                    [marginalia "0.7.0-SNAPSHOT"]
  ;;                    [lein-marginalia "0.7.0-SNAPSHOT"]]

  :cljsbuild
  {:builds
   [{:id "browser-test"
     :source-paths ["src/cljs" "test/cljs"]
     :compiler
     {:target :browser
      :output-to "resources/js/zetta-parser-browser-test.js"
      :externs ["externs/buster.js"]
      :optimizations :whitespace
      :pretty-print true}}
    {:id "browser-optimized-test"
     :source-paths ["src/cljs" "test/cljs"]
     :compiler
     {:target :browser
      :output-to "resources/js/zetta-parser-browser-test.js"
      :externs ["externs/buster.js"]
      :optimizations :advanced
      :pretty-print false}}
    {:id "node-test"
     :source-paths ["src/cljs" "test/cljs"]
     :compiler
     {:target :node
      :output-to "resources/js/zetta-parser-node-test.js"
      :externs ["externs/buster.js"]
      :optimizations :simple
      :pretty-print true}}]})
