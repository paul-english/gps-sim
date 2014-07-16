(defproject gps-sim "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name ""
            :url ""}
  :dependencies [[clatrix "0.3.0"]
                 [midje "1.6.3"]
                 [net.mikera/core.matrix "0.26.0"]
                 [org.clojure/clojure "1.6.0"]]
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
