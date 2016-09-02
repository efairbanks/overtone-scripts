(load-file "./algolib.clj")

(def bpm 165)

(def clk (metronome bpm))

(defn play-beat [clock tick]
  (do (kick)))

(defn play
    [clock tick]
    (let
        [beat (clock)]
      (do (at (clock beat) (play-beat clock tick))
          (def tick (+ tick 1))
          (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
