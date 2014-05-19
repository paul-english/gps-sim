(ns gps-sim.utils.angles-test
  (:use midje.sweet
        gps-sim.utils.angles)
  (:require [clojure.core.matrix :refer [shape]]))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Angles"
    ;; DMS means degrees, minutes, seconds. The program spec is
    ;; actually DMSO: degrees, minutes, seconds, orientation; but I
    ;; didn't want to add the o to my functions.
    (fact "dms->radians"
      (let [radians-matrix (dms->radians [[102123.0 40.0 45.0 55.0 1.0 111.0 50.0 58.0 -1.0 1372.0]
                                                  [107061.5 40.0 46.5 49.5 1.0 111.0 49.0 49.0 -1.0 1874.5]
                                                  [112000.0 40.0 48.0 44.0 1.0 111.0 48.0 40.0 -1.0 2377.0]])]

        (shape radians-matrix) => [3 4]
        radians-matrix => [[102123.0 0.7114883177122995 -1.9521410720820354 1372.0]
                           [107061.5 0.7118979852728371 -1.9518065506420699 1874.5]
                           [112000.0 0.7123076528333746 -1.9514720292021042 2377.0]])
      (let [radians-matrix (dms->radians [[102123.0 40 45 55.0 1 111 50 58.0 -1 1372.0]
                                                  [112000.0 40 48 44.0 1 111 48 40.0 -1 2377.0]])]
        (shape radians-matrix) => [2 4]
        radians-matrix => [[102123.0 0.7114883177122995 -1.9521410720820354 1372.0]
                           [112000.0 0.7123076528333746 -1.9514720292021042 2377.0]]))

    (fact "radians->dms"
      (let [dms-matrix (radians->dms [[102123.0 0.7114883177122995 -1.9521410720820354 1372.0]
                                              [107061.5 0.7118979852728371 -1.9518065506420699 1874.5]
                                              [112000.0 0.7123076528333746 -1.9514720292021042 2377.0]])]
        (shape dms-matrix) => [3 10]
        dms-matrix => [[102123.0 40.0 45.0 54.99999999999261 1 111.0 50.0 57.99999999999841 -1 1372.0]
                       [107061.5 40.0 46.5 49.500000000001023 1 111.0 49.0 49.000000000010004 -1 1874.5]
                       [112000.0 40.0 48.0 43.99999999998386 1 111.0 48.0 39.99999999997044 -1 2377.0]]))



))
