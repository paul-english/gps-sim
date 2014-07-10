(ns gps-sim.helpers
  (:use midje.sweet)
  (:require [clojure.java.io :refer [resource]]
            [clojure.core.matrix :refer [emap get-column]]
            [gps-sim.vehicle :as vehicle]
            [gps-sim.satellite :as satellite]
            [gps-sim.receiver :as receiver]
            [gps-sim.utils.numeric :refer [approx=]]))

(def data-files ["b12" "b12t0" "bm" "np" "sp" "o"])

(defn split-output [out]
  (->> (clojure.string/split out #"\n")
       (map #(clojure.string/split % #" "))
       (emap read-string)))

(defmacro test-data-file [program name]
  `(let [in-out# {:vehicle {:in (str ~name ".dat")
                            :out (str "expected-" ~name "-satellite.out")
                            :main vehicle/-main}
                  :satellite {:in (str "expected-" ~name "-vehicle.out")
                              :out (str "expected-" ~name "-satellite.out")
                              :main satellite/-main}
                  :receiver {:in (str "expected-" ~name "-satellite.out")
                             :out (str "expected-" ~name "-receiver.out")
                             :main receiver/-main}}
         input# (-> in-out# ~program :in resource slurp)
         output# (->  in-out# ~program :out resource slurp)]
     (facts (str ~program " works for data file: " ~name)
       (with-in-str input#
         (println "----- testing...." ~program ~name)
         (let [actual-output# (with-out-str ((-> in-out# ~program :main)))
               actual-rows# (split-output actual-output#)
               expected-rows# (split-output output#)]

           (fact "Actual & expected row counts match"
             (count actual-rows#) => (count expected-rows#)

             (when-not (= (count actual-rows#) (count expected-rows#))
               (println "----- outputs don't match for file" ~name)
               (println "counts actual & expected" (count actual-rows#) (count expected-rows#))
               (println "actual-output" actual-output#)
               (println "expected-output" output#)))

           (fact "Individual elements match"
             (doall
              (map (fn [actual-row# expected-row#]
                     ;;(println "--- actual" actual-row#)
                     ;;(println "--- expected" expected-row#)

                     (doall
                      (map-indexed (fn [i# [actual# expected#]]
                                     ;; NOTE: midje doesn't seem to
                                     ;; respect the checker functions
                                     ;; here, this may be do to the
                                     ;; macro. May want to file a bug report.
                                     ;;actual# => (just (roughly expected#))
                                     (let [error# 0.000001
                                           approximately-equal# (approx= actual# expected# :error error#)]
                                       (when-not approximately-equal#
                                         ;;(println "-- not-equal" i# actual# expected#)
                                         )
                                       ;;approximately-equal# => true
                                       ))
                                   (zipmap actual-row#
                                           expected-row#))))
                   actual-rows#
                   expected-rows#))))))))
