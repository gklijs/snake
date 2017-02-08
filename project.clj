(defproject snake "0.1.0-SNAPSHOT"

  :description "FIXME: write description"
  :url "http://example.com/FIXME"

  :dependencies [[bouncer "1.0.0"]
                 [ch.qos.logback/logback-classic "1.1.7"]
                 [compojure "1.5.1"]
                 [cprop "0.1.9"]
                 [luminus-http-kit "0.1.4"]
                 [luminus-nrepl "0.1.4"]
                 [luminus/ring-ttl-session "0.3.1"]
                 [markdown-clj "0.9.90"]
                 [metosin/ring-http-response "0.8.0"]
                 [mount "0.1.10"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293" :scope "provided"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.webjars.bower/tether "1.3.7"]
                 [org.webjars/bootstrap "4.0.0-alpha.5"]
                 [org.webjars/font-awesome "4.6.3"]
                 [reagent "0.6.0"]
                 [re-frame "0.8.0"]
                 [com.cognitect/transit-cljs "0.8.239"]
                 [ring-middleware-format "0.7.0"]
                 [ring-webjars "0.1.1"]
                 [ring/ring-defaults "0.2.1"]
                 [selmer "1.10.0"]]

  :min-lein-version "2.0.0"

  :jvm-opts ["-server" "-Dconf=.lein-env"]
  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]
  :target-path "target/%s/"
  :main snake.core

  :plugins [[lein-cprop "1.0.1"]
            [lein-cljsbuild "1.1.4"]]
  :clean-targets ^{:protect false}
[:target-path [:cljsbuild :builds :app :compiler :output-dir] [:cljsbuild :builds :app :compiler :output-to]]
  :figwheel
  {:http-server-root "public"
   :nrepl-port       7002
   :css-dirs         ["resources/public/css"]
   :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}


  :profiles
  {:uberjar       {:omit-source    true
                   :prep-tasks     ["compile" ["cljsbuild" "once" "min"]]
                   :cljsbuild
                                   {:builds
                                    {:min
                                     {:source-paths ["src/cljc" "src/cljs" "env/prod/cljs"]
                                      :compiler
                                                    {:output-to     "target/cljsbuild/public/js/app.js"
                                                     :externs       ["react/externs/react.js"]
                                                     :optimizations :advanced
                                                     :pretty-print  false
                                                     :closure-warnings
                                                                    {:externs-validation :off :non-standard-jsdoc :off}}}}}


                   :aot            :all
                   :uberjar-name   "snake.jar"
                   :source-paths   ["env/prod/clj"]
                   :resource-paths ["env/prod/resources"]}

   :dev           [:project/dev :profiles/dev]
   :test          [:project/dev :project/test :profiles/test]

   :project/dev   {:dependencies   [[prone "1.1.2"]
                                    [ring/ring-mock "0.3.0"]
                                    [ring/ring-devel "1.5.0"]
                                    [pjstadig/humane-test-output "0.8.1"]
                                    [binaryage/devtools "0.8.2"]
                                    [com.cemerick/piggieback "0.2.2-SNAPSHOT"]
                                    [doo "0.1.7"]
                                    [figwheel-sidecar "0.5.8"]]
                   :plugins        [[com.jakemccrary/lein-test-refresh "0.14.0"]
                                    [lein-doo "0.1.7"]
                                    [lein-figwheel "0.5.8"]
                                    [org.clojure/clojurescript "1.9.293"]]
                   :cljsbuild
                                   {:builds
                                    {:app
                                     {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                                      :compiler
                                                    {:main          "snake.app"
                                                     :asset-path    "/js/out"
                                                     :output-to     "target/cljsbuild/public/js/app.js"
                                                     :output-dir    "target/cljsbuild/public/js/out"
                                                     :source-map    true
                                                     :optimizations :none
                                                     :pretty-print  true}}}}



                   :doo            {:build "test"}
                   :source-paths   ["env/dev/clj" "test/clj"]
                   :resource-paths ["env/dev/resources"]
                   :repl-options   {:init-ns user}
                   :injections     [(require 'pjstadig.humane-test-output)
                                    (pjstadig.humane-test-output/activate!)]}
   :project/test  {:resource-paths ["env/test/resources"]
                   :cljsbuild
                                   {:builds
                                    {:test
                                     {:source-paths ["src/cljc" "src/cljs" "test/cljs"]
                                      :compiler
                                                    {:output-to     "target/test.js"
                                                     :main          "snake.doo-runner"
                                                     :optimizations :whitespace
                                                     :pretty-print  true}}}}

                   }
   :profiles/dev  {}
   :profiles/test {}})
