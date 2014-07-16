(ns gps-sim.utils.coordinates-test
  (:use midje.sweet
        gps-sim.utils.coordinates)
  (:require [clojure.core.matrix :refer [shape]]))

(facts "Coordinates"
  ;; DMS means degrees, minutes, seconds. The program spec is
  ;; actually DMSO: degrees, minutes, seconds, orientation; but I
  ;; didn't want to add the o to my functions.
  (fact "dms->radians"
    (let [radians-matrix (dms->radians [[102123.0 40 45 55.0 1 111 50 58.0 -1 1372.0]
                                        [107061.5 40 47 19.5 1 111 49 49.0 -1 1874.5]
                                        [112000.0 40 48 44.0 1 111 48 40.0 -1 2377.0]])]

      (shape radians-matrix) => [3 4]
      radians-matrix => (just
                         [(just [(roughly 102123) (roughly 0.71148) (roughly -1.9521) (roughly 1372)])
                          (just [(roughly 107061) (roughly 0.71189) (roughly -1.9518) (roughly 1874)])
                          (just [(roughly 112000) (roughly 0.71230) (roughly -1.9514) (roughly 2377)])]))
    (let [radians-matrix (dms->radians [[102123.0 40 45 55.0 1 111 50 58.0 -1 1372.0]
                                        [112000.0 40 48 44.0 1 111 48 40.0 -1 2377.0]])]
      (shape radians-matrix) => [2 4]
      radians-matrix => (just [(just [(roughly 102123) (roughly 0.71148) (roughly -1.9521) (roughly 1372)])
                               (just [(roughly 112000) (roughly 0.71230) (roughly -1.9514) (roughly 2377)])])))

  (fact "radians->dms"
    (let [dms-matrix (radians->dms [[102123.0 0.7114883177122995 -1.9521410720820354 1372.0]
                                    [107061.5 0.7118979852728371 -1.9518065506420699 1874.5]
                                    [112000.0 0.7123076528333746 -1.9514720292021042 2377.0]])]
      (shape dms-matrix) => [3 10]
      dms-matrix => (just [(just [(roughly 102123.0) 40 45 (roughly 55.0) 1 111 50 (roughly 58) -1 (roughly 1372.0)])
                           (just [(roughly 107061.5) 40 47 (roughly 19.5) 1 111 49 (roughly 49) -1 (roughly 1874.5)])
                           (just [(roughly 112000.0) 40 48 (roughly 44.0) 1 111 48 (roughly 40) -1 (roughly 2377.0)])])))

  (fact "dms->radians->dms"
    (radians->dms
     (dms->radians [[102123.0 40 45 55.0 1 111 50 58.0 -1 1372.0]
                    [107061.5 40 47 19.5 1 111 49 49.0 -1 1874.5]
                    [112000.0 40 48 44.0 1 111 48 40.0 -1 2377.0]]))
    => (just [(just [(roughly 102123.0 0.1) 40 45 (roughly 55 0.1) 1 111 50 (roughly 58 0.1) -1 (roughly 1372 0.1)])
              (just [(roughly 107061.5 0.1) 40 47 (roughly 19.5 0.1) 1 111 49 (roughly 49 0.1) -1 (roughly 1874.5 0.1)])
              (just [(roughly 112000.0 0.1) 40 48 (roughly 44 0.1) 1 111 48 (roughly 40 0.1) -1 (roughly 2377 0.1)])]))

  (fact "radians->dms->radians"
    (dms->radians
     (radians->dms [[102123.0 0.7114883177122995 -1.9521410720820354 1372.0]
                    [107061.5 0.7118979852728371 -1.9518065506420699 1874.5]
                    [112000.0 0.7123076528333746 -1.9514720292021042 2377.0]]))
    => (just
        [(just [(roughly 102123.0 0.1) (roughly 0.71148 0.1) (roughly -1.9521 0.1) (roughly 1372.0 0.1)])
         (just [(roughly 107061.5 0.1) (roughly 0.71190 0.1) (roughly -1.9518 0.1) (roughly 1874.5 0.1)])
         (just [(roughly 112000.0 0.1) (roughly 0.71230 0.1) (roughly -1.9514 0.1) (roughly 2377.0 0.1)])]))

  (fact "rad->cartesian"
    (rad->cartesian [[2.0 1.0 1.0 1.0]
                     [3.0 1.2 1.0 1.0]
                     [2.5 1.0 1.0 0.3]])
    => (just [(just [(roughly 1858826.5991) (roughly 2894950.9043) (roughly 5358020.6355)])
              (just [(roughly 1246635.8649) (roughly 1941520.3259) (roughly 5934708.0837)])
              (just [(roughly 1858826.3947) (roughly 2894950.5860) (roughly 5358020.0465)])]))

  ;; TODO test for x, or y = 0

  (fact "cartesian->rad"
    (cartesian->rad [0.0 0.0 0.0]
                    [[1858826.5991443316 2894950.904301383 5358020.6355956085]
                     [1246635.8649815454 1941520.3259541045 5934708.083766128]
                     [1858826.3947957244 2894950.586047284 5358020.046565919]])
    => (just [(just [(roughly 0) (roughly 1) (roughly 1) (roughly 1)])
              (just [(roughly 0) (roughly 1.2) (roughly 1) (roughly 1)])
              (just [(roughly 0) (roughly 1) (roughly 1) (roughly 0.3)])])

    (cartesian->rad [12123.967274951485]
                    [[-1795214.6053696016 -4477178.644777841 4158593.453213802]])
    => (just [[12123.967274951485 0.7114883177775478 4.331046621493604 1371.9996096240357]]))

  (fact "rad->cartesian->rad"
    (cartesian->rad [2.0 3.0 2.5] (rad->cartesian [[2.0 1.0 1.0 1.0]
                                                   [3.0 1.2 1.0 1.0]
                                                   [2.5 1.0 1.0 0.3]]))
    => (just [(just [(roughly 2) (roughly 1) (roughly 1) (roughly 1)])
              (just [(roughly 3) (roughly 1.2) (roughly 1) (roughly 1)])
              (just [(roughly 2.5) (roughly 1) (roughly 1) (roughly 0.3)])])))
