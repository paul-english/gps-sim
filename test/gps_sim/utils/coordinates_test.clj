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
        radians-matrix => (just
                           [(just [(roughly 102123) (roughly 0.71148) (roughly -1.9521) (roughly 1372)])
                            (just [(roughly 107061) (roughly 0.71189) (roughly -1.9518) (roughly 1874)])
                            (just [(roughly 112000) (roughly 0.71230) (roughly -1.9514) (roughly 2377)])]))
      (let [radians-matrix (dms->radians [[102123.0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]
                                          [112000.0M 40 48 44.0M 1 111 48 40.0M -1 2377.0M]])]
        (shape radians-matrix) => [2 4]
        radians-matrix => (just [(just [(roughly 102123) (roughly 0.71148) (roughly -1.9521) (roughly 1372)])
                                 (just [(roughly 112000) (roughly 0.71230) (roughly -1.9514) (roughly 2377)])])))

    (fact "radians->dms"
      (let [dms-matrix (radians->dms [[102123.0M 0.7114883177122995M -1.9521410720820354M 1372.0M]
                                      [107061.5M 0.7118979852728371M -1.9518065506420699M 1874.5M]
                                      [112000.0M 0.7123076528333746M -1.9514720292021042M 2377.0M]])]
        (shape dms-matrix) => [3 10]
        dms-matrix => (just [(just [(roughly 102123.0) 40 45 (roughly 55.0) 1 111 50 (roughly 58) -1 (roughly 1372.0)])
                             (just [(roughly 107061.5) 40 47 (roughly 19.5) 1 111 49 (roughly 49) -1 (roughly 1874.5)])
                             (just [(roughly 112000.0) 40 48 (roughly 44.0) 1 111 48 (roughly 40) -1 (roughly 2377.0)])])))

    (fact "dms->radians->dms"
      (radians->dms
       (dms->radians [[102123.0M 40 45 55.0M 1 111 50 58.0M -1 1372.0M]
                      [107061.5M 40 47 19.5M 1 111 49 49.0M -1 1874.5M]
                      [112000.0M 40 48 44.0M 1 111 48 40.0M -1 2377.0M]]))
      => (just [(just [(roughly 102123.0) 40 45 (roughly 55) 1 111 50 (roughly 58) -1 (roughly 1372)])
                (just [(roughly 107061.5) 40 47 (roughly 19.5) 1 111 49 (roughly 49) -1 (roughly 1874.5)])
                (just [(roughly 112000.0) 40 48 (roughly 44) 1 111 48 (roughly 40) -1 (roughly 2377)])]))

    (fact "radians->dms->radians"
      (dms->radians
       (radians->dms [[102123.0M 0.7114883177122995M -1.9521410720820354M 1372.0M]
                      [107061.5M 0.7118979852728371M -1.9518065506420699M 1874.5M]
                      [112000.0M 0.7123076528333746M -1.9514720292021042M 2377.0M]]))
      => (just
          [(just [(roughly 102123) (roughly 0.71148) (roughly -1.9521) (roughly 1372)])
           (just [(roughly 107061) (roughly 0.71189) (roughly -1.9518) (roughly 1874)])
           (just [(roughly 112000) (roughly 0.71230) (roughly -1.9514) (roughly 2377)])]))

    (fact "rad->cartesian"
      (rad->cartesian [[2M 1M 1M 1M]
                       [3M 1.2M 1M 1M]
                       [2.5M 1M 1M 0.3M]])
      => (just [(just [(roughly 1858826.5991) (roughly 2894950.9043) (roughly 5358020.6355)])
                (just [(roughly 1246635.8649) (roughly 1941520.3259) (roughly 5934708.0837)])
                (just [(roughly 1858826.3947) (roughly 2894950.5860) (roughly 5358020.0465)])]))

    (fact "cartesian->rad"
      (cartesian->rad [0M 0M 0M]
                      [[1858826.5991443316M 2894950.904301383M 5358020.6355956085M]
                       [1246635.8649815454M 1941520.3259541045M 5934708.083766128M]
                       [1858826.3947957244M 2894950.586047284M 5358020.046565919M]])
      => (just [(just [(roughly 0) (roughly 1) (roughly 1) (roughly 1)])
                (just [(roughly 0) (roughly 1.2) (roughly 1) (roughly 1)])
                (just [(roughly 0) (roughly 1) (roughly 1) (roughly 0.3)])]))

    (fact "rad->cartesian->rad"
      (cartesian->rad [2M 3M 2.5M] (rad->cartesian [[2M 1M 1M 1M]
                                                    [3M 1.2M 1M 1M]
                                                    [2.5M 1M 1M 0.3M]]))
      => (just [(just [(roughly 2) (roughly 1) (roughly 1) (roughly 1)])
                (just [(roughly 3) (roughly 1.2) (roughly 1) (roughly 1)])
                (just [(roughly 2.5) (roughly 1) (roughly 1) (roughly 0.3)])]))))
