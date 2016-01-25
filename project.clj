(defproject calc_angle "0.1.0-SNAPSHOT"
            :description "calc angles for machine arm"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}

            :dependencies [[org.clojure/clojure "1.7.0"]
                           [org.clojure/clojurescript "1.7.170"]
                           [ring/ring-defaults "0.1.5" :exclusions [javax.servlet/servlet-api]]
                           [ring/ring-core "1.4.0"]
                           [compojure "1.4.0"]
                           [org.scream3r/jssc "2.8.0"]
                           [cljs-ajax "0.5.3"]]

            :plugins [[lein-cljsbuild "1.1.1"]
                      [lein-figwheel "0.5.0-3"]
                      [lein-ring "0.9.7"]]

            :source-paths ["src/clj"]

            :clean-targets ^{:protect false} ["resources/public/js/" "target"]

            :cljsbuild {:builds
                        [{:id "dev"
                          :source-paths ["src/cljs"]
                          :figwheel {:on-jsload "handwrite_strokes.core/on-js-reload"}
                          :compiler {:main handwrite_strokes.core
                                     :asset-path "js/out"
                                     :output-to "resources/public/js/handwrite_strokes.js"
                                     :output-dir "resources/public/js/out"
                                     :source-map-timestamp true}}
                         ;; This next build is an compressed minified build for
                         ;; production. You can build this with:
                         ;; lein cljsbuild once min
                         {:id "min"
                          :source-paths ["src/cljs"]
                          :compiler {:output-to "resources/public/js/handwrite_strokes.js"
                                     :main handwrite_strokes.core
                                     :optimizations :advanced
                                     :pretty-print false}}]}

            :ring {:handler calc-angle.core/entry 
                   :init calc-angle.core/init 
                   :destroy calc-angle.core/destroy}
            :main ^:skip-aot calc-angle.core
            :profiles {:uberjar {:aot :all}})

