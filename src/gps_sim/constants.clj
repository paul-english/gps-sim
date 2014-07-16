(ns gps-sim.constants)

(def ^:dynamic pi (atom Math/PI))
(def ^:dynamic tau (atom (* 2 @pi)))
(def ^:dynamic c (atom 299792458))
(def ^:dynamic R (atom 6367444.5))
(def ^:dynamic s (atom 86164.09))

(defn set-constants! [constants]
  (reset! pi (:pi constants))
  (reset! tau (* 2 @pi))
  (reset! c (:c constants))
  (reset! R (:R constants))
  (reset! s (:s constants)))

(defn read-constants! [data]
  (set-constants! (zipmap [:pi :c :R :s]
                          data)))
