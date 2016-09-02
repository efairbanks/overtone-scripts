(load-file "./algolib.clj")

(def bpm 140)
(def clk (metronome bpm))

(definst sin-pad [freq 440 amp (db->amp -20)]
  (let [env (env-gen (envelope [0 1 0] [0.05 2]) :action FREE)
        left (sin-osc (- freq (* freq 0.01)) (sin-osc freq))
        right (sin-osc (+ freq (* freq 0.01)) (sin-osc freq))]
    (out 0 (* env amp [left right]))))

(def f-seq [1])
(def fracts [3/2 2/3 4/3 3/4 5/4 4/5])

(count f-seq)

(defn play-beat
  [clock tick]
  (e-set :clock clock)
  (e-set :tick (+ 1 (e-get :tick 0)))
  (let [base-freq 440
        base-freq (* base-freq (wrap-at [1 3 5] (section-index (e-get :tick 0) 32)))
        freq (fn [] (* base-freq (reduce (fn [a b] (* a b)) f-seq)))
        min-freq 300
        max-freq 600
        coef (wrap-at [11 17 13 8 23 7 15 10] (section-index (e-get :tick 0) 4))
        ;coef (wrap-at [1] (section-index (e-get :tick 0) 4))
        ]
    (if (== 0 (section-index (e-get :tick 0) 16 1)) (def f-seq [1]))
    (while (< (freq) min-freq) (def f-seq (doall (concat [2] f-seq))))
    (while (> (freq) max-freq) (def f-seq (doall (concat [0.5] f-seq))))
    (if (< 0 (wrap-at [1 0 0 1 0 0 0 0] (e-get :tick 0))) (sin-pad (* (freq) 0.25)))
    (sin-pad (freq))
    (def f-seq (doall (concat f-seq [(wrap-at fracts (* coef (e-get :tick 0)))])))
    )
  )

(play clk 0 play-beat)
                                        ;(stop)
