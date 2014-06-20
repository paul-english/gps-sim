(defproject gps-sim "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name ""
            :url ""}
  :dependencies [[clj-time "0.7.0"]
                 [midje "1.6.3"]
                 [net.mikera/core.matrix "0.24.0"]
                 [org.clojure/clojure "1.6.0"]
                 [prismatic/schema "0.2.3"]]
  :global-vars {*warn-on-reflection* true
                *unchecked-math* true
                *math-context* (java.math.MathContext. 20)}
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.4"]]
                   :source-paths ["dev"]
                   :aot :all}
             :vehicle {:main gps-sim.vehicle
                       :uberjar-name "vehicle.jar"}
             :receiver {:main gps-sim.receiver
                        :uberjar-name "receiver.jar"}
             :satellite {:main gps-sim.satellite
                         :uberjar-name "satellite.jar"}
             :uberjar {:aot :all
                       :exclusions [gps-sim.core]}})
