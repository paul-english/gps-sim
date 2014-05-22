(ns gps-sim.utils.coordinates-test
  (:use midje.sweet
        gps-sim.utils.coordinates)
  (:require [clojure.core.matrix :refer [shape]]
            [gps-sim.utils.schemas :refer [parse-dms-list]]))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Coordinates"
    ;; DMS means degrees, minutes, seconds. The program spec is
    ;; actually DMSO: degrees, minutes, seconds, orientation; but I
    ;; didn't want to add the o to my functions.
    (fact "dms->radians"
      (let [radians-matrix (dms->radians [[102123.0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]
                                          [107061.5M 40 47 19.5M 1 111 49 49.0M -1 1874.5M]
                                          [112000.0M 40 48 44.0M 1 111 48 40.0M -1 2377.0M]])]

        (shape radians-matrix) => [3 4]
        radians-matrix => [[102123.0M 0.7114883177122994933731062872266540654320M -1.952141072082035292764694255466918691360M 1372.0M]
                           [107061.5M 0.7118979852728370512527050000000000000000M -1.951806550642069713070872872266540654320M 1874.5M]
                           [112000.0M 0.7123076528333746091323037127733459345680M -1.951472029202104133202518563866729672840M 2377.0M]])
      (let [radians-matrix (dms->radians [[102123.0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]
                                          [112000.0M 40 48 44.0M 1 111 48 40.0M -1 2377.0M]])]
        (shape radians-matrix) => [2 4]
        radians-matrix => [[102123.0M 0.7114883177122994933731062872266540654320M -1.952141072082035292764694255466918691360M 1372.0M]
                           [112000.0M 0.7123076528333746091323037127733459345680M -1.951472029202104133202518563866729672840M 2377.0M]]))

    (fact "radians->dms"
      (let [dms-matrix (radians->dms [[102123.0M 0.7114883177122995M -1.9521410720820354M 1372.0M]
                                      [107061.5M 0.7118979852728371M -1.9518065506420699M 1874.5M]
                                      [112000.0M 0.7123076528333746M -1.9514720292021042M 2377.0M]])]
        (shape dms-matrix) => [3 10]
        dms-matrix => [[102123.0000000000000000M 40 45 54.99999999999261M 1 111 50 57.99999999999841M -1 1372.0000000000000000M]
                       [107061.5000000000000000M 40 47 19.500000000001023M 1 111 49 49.00000000006116M -1 1874.5000000000000000M]
                       [112000.0000000000000000M 40 48 44.000000000009436M 1 111 48 40.0000000000216M -1 2377.0000000000000000M]]))

    (fact "rad->cartesian"
      (rad->cartesian [[2M 1M 0M 1M]
                       [3M 3M 1M 1M]
                       [2.5M 1M 1M 0M]]) => [[5358020.6355956085M 0.0M 3440345.48613971M]
                                             [485501.58273276367M 756123.9152801872M -6303723.267512271M]
                                             [2894950.44965267M 4508618.19278225M 3440344.945837404M]])

    (fact "cartesian->rad"
      (cartesian->rad [0M 0M 0M] [[5358020.6355956085M 0.0M 3440345.48613971M]
                                  [485501.58273276367M 756123.9152801872M -6303723.267512271M]
                                  [2894950.44965267M 4508618.19278225M 3440344.945837404M]]) => [[0M 1.0M 0.0M 1.0M]
                                                                                                 [0M 3.0000000000000004M 1.0M 0.9999999990686774M]
                                                                                                 [0M 1.0M 0.9999999999999999M 0.0M]])))
