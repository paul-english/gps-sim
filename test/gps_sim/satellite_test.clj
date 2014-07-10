(ns gps-sim.satellite-test
  (:refer-clojure :exclude [* - + == /])
  (:use midje.sweet
        gps-sim.satellite
        clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [gps-sim.utils.io :refer [file->matrix]]
            [gps-sim.utils.schemas :refer [parse-data parse-dms-list parse-satellite-list]]
            [gps-sim.constants :refer [s R]]
            [gps-sim.helpers :refer [test-data-file data-files]]))

(with-state-changes [(around :facts (schema.macros/with-fn-validation ?form))]
  (facts "Satellite"
    (fact "Program digests stdin"
      (with-in-str "102123.0 40 45 55.0 1 111 50 58.0 -1 1372.0
102232.74 40 45 56.87 1 111 50 56.46 -1 1383.16
102342.48 40 45 58.75 1 111 50 54.93 -1 1394.33
102452.23 40 46 0.63 1 111 50 53.4 -1 1405.5
102561.97 40 46 2.51 1 111 50 51.86 -1 1416.66\n"
        (with-out-str
          (-main) => :ok)))

    (fact "Satellite locations"
      (let [satellites (parse-satellite-list [[ 0 1.000  0.000 0.000  0.000  0.574 0.819 43082.045 20200000.000 0.000]
                                              [ 0 1.000  0.000 0.000  0.000  0.574 0.819 43082.045 20200000.000 1.571]
                                              [ 0 1.000  0.000 0.000  0.000  0.574 0.819 43082.045 20200000.000 3.142]
                                              [ 0 1.000  0.000 0.000  0.000  0.574 0.819 43082.045 20200000.000 4.712]
                                              [ 0 0.500  0.866 0.000 -0.497  0.287 0.819 43082.045 20200000.000 1.000]])]
        (binding [R (atom 0)]
          (satellite-location satellites (repeat 5 0M))
          => (just [(just [(roughly 20200010) (roughly 0) (roughly 0)])
                    (just [(roughly -4114.2007) (roughly 11594805.4995) (roughly 16543807.8468)])
                    (just [(roughly -20200008.3240) (roughly -4723.1023) (roughly -6739.0606)])
                    (just [(roughly -7857.4074) (roughly -11594804.8628) (roughly -16543806.9384)])
                    (just [(roughly -2990811.9962) (roughly 14329967.2783) (roughly 13921134.5701)])]))
        (satellite-location satellites (repeat 5 1M))
        => (just [(just [(roughly 26567444.2174) (roughly 2224.0535) (roughly 3173.3446)])
                  (just [(roughly -9285.7340) (roughly 15249712.2115) (roughly 21758735.7164)])
                  (just [(roughly -26567440.4349) (roughly -8435.9689) (roughly -12036.6874)])
                  (just [(roughly -6459.5570) (roughly -15249712.6922) (roughly -21758736.4023)])
                  (just [(roughly -3936244.5073) (roughly 18844827.7017) (roughly 18311060.2605)])])))

    (fact "rotate-coordinates"
      (let [coordinate-matrix [[2M 1M 1M]
                               [0M 2M 0M]
                               [1M 0M 3M]]
            theta (* @s [1M 0.5M 0.25M])]
        (rotate-coordinates theta coordinate-matrix)
        => (just [(just [(roughly 2) (roughly 1) (roughly 1)])
                  (just [(roughly 0 0.000001) (roughly -2) (roughly 0)])
                  (just [(roughly 0 0.000001) (roughly 1) (roughly 3)])])))

    (fact "above-horizon?"
      (above-horizon? [2324235.487196124M -4226801.805397957M 4158593.4531539655M]
                      [14 34976.66717943274M -6543705.5766621735M 14768997.646611637M -21092314.55058739M])
      => false

      (above-horizon? [2323927.257730319M -4226971.280105327M 4158593.4531539655M]
                      [0 0M 1477645.012869009M -1.5214872308462147E7M 2.172908956016601E7M])
      => true)

    (fact "Program does the right stuff"
      (let [data (-> "data.dat" file->matrix (get-column 0) parse-data)
            input (parse-dms-list [[12123.0 40 45 55.0 1 111 50 58.0 -1 1372.0] [12124.0 40 45 55.0 1 111 50 58.0 -1 1372.0]])
            results (run data input)]
        (shape results) => [16 5]
        (map first results) => [3 4 8 11 14 15 17 20
                                3 4 8 11 14 15 17 20]

        results => (just [(just [3 (roughly 12122.9172) (roughly 26052343.13778725) (roughly 2986153.9652697924) (roughly 4264669.833325115)])
                          (just [4 (roughly 12122.9181) (roughly -17183556.33086311) (roughly -1.8640834276186436E7) (roughly 7941901.319733662)])
                          (just [8 (roughly 12122.9151) (roughly 18498279.256616846) (roughly -1.4172390064384513E7) (roughly -1.2758766855293432E7)])
                          (just [11 (roughly 12122.929) (roughly -2903225.4285143306) (roughly -1.9661358537802488E7) (roughly 1.7630410370147068E7)])
                          (just [14 (roughly 12122.9308) (roughly 1477645.012869009) (roughly -1.5214872308462147E7) (roughly 2.172908956016601E7)])
                          (just [15 (roughly 12122.9155) (roughly 26526323.652830362) (roughly 847508.5779779141) (roughly -1210367.686336006)])
                          (just [17 (roughly 12122.9321) (roughly 4939777.113795485) (roughly -1.796566328317718E7) (roughly 1.893839095916287E7)])
                          (just [20 (roughly 12122.9302) (roughly 17903461.11594521) (roughly -1.680512822049418E7) (roughly 1.0143118495964047E7)])
                          (just [3 (roughly 12123.9172) (roughly 26051583.57529224) (roughly 2988333.2598188072) (roughly 4267782.188491394)])
                          (just [4 (roughly 12123.9181) (roughly -17182471.209298465) (roughly -1.8643093190170433E7) (roughly 7938946.196116364)])
                          (just [8 (roughly 12123.9151) (roughly 18498702.477611426) (roughly -1.4169522466848126E7) (roughly -1.2761337977143275E7)])
                          (just [11 (roughly 12123.9294) (roughly -2900527.5635450673) (roughly -1.9663425257925782E7) (roughly 1.7628549408265613E7)])
                          (just [14 (roughly 12123.9308) (roughly 1481513.6606823928) (roughly -1.5214748538998244E7) (roughly 2.1728912799052842E7)])
                          (just [15 (roughly 12123.9155) (roughly 26526107.876559697) (roughly 849727.5371656624) (roughly -1213536.6884770312)])
                          (just [17 (roughly 12123.9321) (roughly 4942411.176984259) (roughly -1.796329047680593E7) (roughly 1.8939954441975158E7)])
                          (just [20 (roughly 12123.9302) (roughly 17904260.819928236) (roughly -1.680258096219933E7) (roughly 1.0145926510738155E7)])])))

    (future-facts "Satellite generates the right output for each data file"
      (doseq [data-file data-files]
        (test-data-file :satellite data-file)))))
