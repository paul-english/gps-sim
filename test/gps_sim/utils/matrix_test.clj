(ns gps-sim.utils.matrix-test
  (:use midje.sweet
        gps-sim.utils.matrix))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Matrix"
    (fact "We can perform linear interpolation on matrices"
      (lerp [[1] [2]] 0.1) => [[1.0] [1.1] [1.2] [1.3] [1.4] [1.5] [1.6] [1.7] [1.8] [1.9] [2.0]]
      (lerp [[1] [1]] 0.1) => [[1.0] [1.0] [1.0] [1.0] [1.0] [1.0] [1.0] [1.0] [1.0] [1.0] [1.0]]
      (lerp [[1 2] [2 3]] 0.1) => [[1.0 2.0]
                                   [1.1 2.1]
                                   [1.2 2.2]
                                   [1.3 2.3]
                                   [1.4 2.4]
                                   [1.5 2.5]
                                   [1.6 2.6]
                                   [1.7 2.7]
                                   [1.8 2.8]
                                   [1.9 2.9]
                                   [2.0 3.0]]
      (lerp [[102123.0 40 45] [112000.0 40 48]] 0.1) => [[102123.0 40.0 45.0]
                                                         [103110.7 40.0 45.3]
                                                         [104098.4 40.0 45.6]
                                                         [105086.1 40.0 45.9]
                                                         [106073.8 40.0 46.2]
                                                         [107061.5 40.0 46.5]
                                                         [108049.2 40.0 46.8]
                                                         [109036.9 40.0 47.1]
                                                         [110024.6 40.0 47.4]
                                                         [111012.3 40.0 47.7]
                                                         [112000.0 40.0 48.0]]
      (lerp [[102123.000 40.000 45.000 55.000 1.000 111.000 50.000 58.000 -1.000 1372.000]
             [112000.000 40.000 48.000 44.000 1.000 111.000 48.000 40.000 -1.000 2377.000]]
            0.5)
      => [[102123.0 40.0 45.0 55.0 1.0 111.0 50.0 58.0 -1.0 1372.0]
          [107061.5 40.0 46.5 49.5 1.0 111.0 49.0 49.0 -1.0 1874.5]
          [112000.0 40.0 48.0 44.0 1.0 111.0 48.0 40.0 -1.0 2377.0]])
    (fact "join-1"
      (join-1 [[1] [2]] [[3] [4]] [[5] [6]]) => [[1 3 5]
                                                 [2 4 6]])
    (fact "join-1-interleave"
      (join-1-interleave [[1 2] [3 4]]
                         [[5 6] [7 8]]
                         [[9 10] [11 12]])
      => [[1 5 9 2 6 10]
          [3 7 11 4 8 12]])

    (fact "rotation transform matrix"
      (rotation-matrix (bigdec 1/8)) => [[0.7071067811865476 -0.7071067811865475 0]
                                         [0.7071067811865475 0.7071067811865476 0]
                                         [0 0 1]]
      (rotation-matrix (bigdec 1/2)) => [[-1.0 -1.2246467991473532E-16 0]
                                         [1.2246467991473532E-16 -1.0 0]
                                         [0 0 1]])

    (fact "scaling transform matrix"
      (scaling-matrix 3 [2 2]) => [[3.0 0.0]
                                   [0.0 3.0]])))
