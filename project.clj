(defproject cliculator "0.1.0-SNAPSHOT"
  :description "Simple CLI calculator"
  :url "https://github.com/mathrick/cliculator"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.2"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [instaparse "1.4.12"]
                 [cli-matic "0.5.4"]]
  :main ^:skip-aot cliculator.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-shell "0.5.0"]]}}
  :aliases
  {"native"
   ["shell"
    "native-image" "--report-unsupported-elements-at-runtime"
    "--initialize-at-build-time" "--no-server"
    "-jar" "target/uberjar/${:uberjar-name:-${:name}-${:version}-standalone.jar}"
    "-H:Name=./target/${:name}"]})
