(defproject ppm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/main/clojure"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [net.mikera/core.matrix "0.49.0"]
                 [net.mikera/vectorz-clj "0.43.0"]
                 [net.mikera/imagez "0.10.0"]
                 [net.mikera/clojure-utils "0.6.2"]
                 [org.imgscalr/imgscalr-lib "4.2"]
                 [net.mikera/mathz "0.3.0"]
                 [net.mikera/mikera-gui "0.3.1"]
                 [net.mikera/randomz "0.3.0"]
                 [me.raynes/conch "0.8.0"]
                 ]
  :main ^:skip-aot ppm.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

