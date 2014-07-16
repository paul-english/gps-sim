(ns gps-sim.receiver-test
  (:refer-clojure :exclude [* - + == /])
  (:use midje.sweet
        gps-sim.receiver
        clojure.core.matrix
        clojure.core.matrix.operators)
  (:require [clojure.java.io :refer [resource]]
            [gps-sim.utils.io :refer [file->matrix]]
            [gps-sim.helpers :refer [test-data-file data-files split-output]]))

(def input [[3 12122.917273538935 2.605234313778725E7 2986153.9652697924 4264669.833325115]
            [4 12122.918115974104 -1.718355633086311E7 -1.8640834276186436E7 7941901.319733662]
            [8 12122.91517247339 1.8498279256616846E7 -1.4172390064384513E7 -1.2758766855293432E7]
            [11 12122.929474004011 -2903225.4285143306 -1.9661358537802488E7 1.7630410370147068E7]
            [14 12122.93081680465 1477645.012869009 -1.5214872308462147E7 2.172908956016601E7]
            [15 12122.91559232703 2.6526323652830362E7 847508.5779779141 -1210367.686336006]
            [17 12122.932126735379 4939777.113795485 -1.796566328317718E7 1.893839095916287E7]
            [20 12122.9302901758 1.790346111594521E7 -1.680512822049418E7 1.0143118495964047E7]
            [3 12123.917274951486 2.605158357529224E7 2988333.2598188072 4267782.188491394]
            [4 12123.91811479538 -1.7182471209298465E7 -1.8643093190170433E7 7938946.196116364]
            [8 12123.915170042981 1.8498702477611426E7 -1.4169522466848126E7 -1.2761337977143275E7]
            [11 12123.929474483639 -2900527.5635450673 -1.9663425257925782E7 1.7628549408265613E7]
            [14 12123.930817706947 1481513.6606823928 -1.5214748538998244E7 2.1728912799052842E7]
            [15 12123.91559038413 2.6526107876559697E7 849727.5371656624 -1213536.6884770312]
            [17 12123.932126911204 4942411.176984259 -1.796329047680593E7 1.8939954441975158E7]
            [20 12123.930291044038 1.7904260819928236E7 -1.680258096219933E7 1.0145926510738155E7]])

(facts "Receiver"
  (fact "Program digests stdin"
    (with-in-str "3 12122.917273538935 2.605234313778725E7 2986153.9652697924 4264669.833325115
4 12122.918115974104 -1.718355633086311E7 -1.8640834276186436E7 7941901.319733662
8 12122.91517247339 1.8498279256616846E7 -1.4172390064384513E7 -1.2758766855293432E7
11 12122.929474004011 -2903225.4285143306 -1.9661358537802488E7 1.7630410370147068E7
14 12122.93081680465 1477645.012869009 -1.5214872308462147E7 2.172908956016601E7
15 12122.91559232703 2.6526323652830362E7 847508.5779779141 -1210367.686336006
17 12122.932126735379 4939777.113795485 -1.796566328317718E7 1.893839095916287E7
20 12122.9302901758 1.790346111594521E7 -1.680512822049418E7 1.0143118495964047E7
3 12123.917274951486 2.605158357529224E7 2988333.2598188072 4267782.188491394
4 12123.91811479538 -1.7182471209298465E7 -1.8643093190170433E7 7938946.196116364
8 12123.915170042981 1.8498702477611426E7 -1.4169522466848126E7 -1.2761337977143275E7
11 12123.929474483639 -2900527.5635450673 -1.9663425257925782E7 1.7628549408265613E7
14 12123.930817706947 1481513.6606823928 -1.5214748538998244E7 2.1728912799052842E7
15 12123.91559038413 2.6526107876559697E7 849727.5371656624 -1213536.6884770312
17 12123.932126911204 4942411.176984259 -1.796329047680593E7 1.8939954441975158E7
20 12123.930291044038 1.7904260819928236E7 -1.680258096219933E7 1.0145926510738155E7\n"
      (with-out-str
        (-main) => :ok)))

  (fact "We can group the receiver input by the number of path points we have"
    (let [grouped-satellites (group-by-index-change input)]
      (count grouped-satellites) => 2
      (->> grouped-satellites
           first
           (map first)) => [3 4 8 11 14 15 17 20]
           (->> grouped-satellites
                second
                (map first)) => [3 4 8 11 14 15 17 20]))

  (fact "Program does the right stuff"
    (let [data (-> "data.dat" file->matrix (get-column 0))
          results (run data input)]
      results => (just [(just [(roughly 12123.0) 40 45 (roughly 55.0) 1 111 50 (roughly 58.0) -1 (roughly 1372.0)])
                        (just [(roughly 12124.0) 40 45 (roughly 55.0) 1 111 50 (roughly 58.0) -1 (roughly 1371.99)])])))

  (fact "Receiver doesn't blow up at the Poles"
    (println "--------------------------------------------------")
    (println "testing receiver at NP")
    (println "--------------------------------------------------")
    (let [data (-> "data.dat" file->matrix (get-column 0))
          input (-> "expected-np-satellite.out"
                    resource
                    slurp
                    split-output)
          results (run data input)]
      results => anything))

  (future-facts "Receiver generates the right output for each data file"
                (doseq [data-file data-files]
                  (test-data-file :receiver data-file))))
