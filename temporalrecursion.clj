(use 'overtone.live)
(use 'overtone.helpers.file)
(use '[clojure.string :as str])

                                        ; making a function
(defn hello-world [] (println "Hello World!"))
                                        ; run it
(hello-world)
                                        ; making a function with args
(defn print-my-thing [thing] (println thing))
                                        ; run it
(print-my-thing "stuff")
                                        ; making a synth
(definst test-tone [] (sin-osc))
                                        ; run it
(test-tone)
                                        ; making a noise that stops
(definst test-tone
  []
  (out 0 (* (sin-osc 440)
            (env-gen (envelope [0 1 0] [0.01 0.1]) :action FREE))))
                                        ; run it
(test-tone)
                                        ; make a scheduler
(defn play
  [time]
  (do (test-tone)
      (apply-by (+ time 200) play [(+ time 200)])))
                                        ; run it!
(play (now))
                                        ; keeping time with a metronome is better and easier
(def clk (metronome 120))
                                        ; what's this do?
(clk)
                                        ; ten beats from now
(- (clk (+ 10 (clk))) (now))
                                        ; oh cool so it counts in the background
                                        ; lets use it
(println (clk (+ 1 (clk))))
