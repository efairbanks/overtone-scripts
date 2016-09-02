(load-file "./algolib.clj")

(def vocals (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
(def house (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/house")))
(def openhats (load-sample-dir "/sounds/openhats"))

(def bpm (* 160 1))
(def clk (metronome bpm))

(definst chimes [freq 440 amp 1 outbus 0]
  (let [env (env-gen (envelope [0 1 (db->amp -25) 0] [0.001 0.05 0.5]) :action FREE)
        mod (mix (sin-osc (* [2 4 8] freq)))
        output (* (sin-osc (+ freq (* mod 1000)))
                  (sin-osc (* freq 2.005))
                  (sin-osc (* freq 3.007))
                  (sin-osc (* freq 6.997))
                  env)
        output (* output amp)
        output [output output]]
    (out outbus output)))

(do
  (definst sidechain [carrierbus 0 modbus 0 outbus 0]
    (let [carrier (in carrierbus 2)
          ;carrier (compander carrier carrier (db->amp -30) 1 0.2 0.001 0.03)
          carrier (* (db->amp 20) carrier)
          carrier (free-verb carrier 0.15 0.9 0.3)
          mod (in modbus 2)
          output (compander carrier mod (db->amp -30) 1 0.5 0.001 0.03)
          output (+ output mod)]
      (out outbus output)))
  (kill sidechain)
  (def sidechain-carrierbus (audio-bus 2))
  (def sidechain-modbus (audio-bus 2))
  (def sidechain-fx (sidechain sidechain-carrierbus
                               sidechain-modbus
                               0)))

(def chords
  [
   (chord :f4 :major)
   (chord :a4 :minor)
   (chord :g4 :major)
   (chord :c4 :major)
   (chord :f4 :major)
   (chord :a4 :sus4)
   (chord :g4 :sus4)
   (chord :d4 :minor7)
   (chord :f4 :major)
   (chord :a4 :minor)
   (chord :g4 :major)
   (chord :c4 :major)
   (chord :f4 :major)
   (chord :a4 :sus2)
   (chord :g4 :sus4)
   (chord :e4 :augmented)
   ])

(comment
  (do (kill ploop-grain)
      (ploop-grain
       (wrap-at vocals 0)
       1 1 0 0.5)))

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index tick 8))
        section (section-index tick 32)]

    (comment)
    (let ;percussion
        [
         kick-rhythm (wrap-at [[]  (E 2 8)]
                              (wrap-at [0 0 0 0 0 0
                                        1 1 1 1 1 1] section))
         snare-rhythm (wrap-at [[]
                                (shr (E 8 32) (wrap-at [2 2 2 2
                                                        2 2 2 2
                                                        2 2 2 2
                                                        2 2 2 2
                                                        2 2 2 2
                                                        2 2 2 0]
                                                       (section-index tick 8)))
                                ]
                               (wrap-at [0 0 0 0 0 1
                                         1 1 1 1 1 1] section))
         hats-rhythm (wrap-at [[] (shr (E 4 8) 0)]
                              (wrap-at [0 0 0 0 0 0
                                        1 1 1 1 1 1] section))
         snare-speed (wrap-at [16 16 16 16
                               16 16 16 16
                               16 16 16 16
                               16 16 16 16
                               16 16 16 16
                               16 16 16
                               (wrap-at [8 4 2 1] (section-index tick 2))]
                              (section-index tick 8))
         ]
      (play-rhythm-for-tick
       kick-rhythm
       2
       tick
       clock
       psamp
       [:buf (wrap-at house 0)
        :outbus sidechain-modbus])

      (play-rhythm-for-tick
       snare-rhythm
       snare-speed
       tick
       clock
       psamp
       [:buf (wrap-at house 1)
        :amp (db->amp 0)])

      (play-rhythm-for-tick
       hats-rhythm
       2
       tick
       clock
       psamp
       [:buf (wrap-at openhats 0)
        :offset 0
        :amp (db->amp -24)
        :outbus sidechain-carrierbus]))

                                        ; --- ;

    (let ;bass
        [bass-rhythm (wrap-at [[]  (shr (E 4 16) 2)]
                              (wrap-at [0 0 0 0 0 0
                                        1 1 1 1 1 1] section))]
      (play-chord-rhythm-for-tick
       bass-rhythm
       4
       tick
       clock
       super-saw
       (xpose-notes [(second chord)] 31)
       :bass
       [:outbus sidechain-carrierbus
        :amp (db->amp -9)
        :width 1.2
        :attack 0.015
        :decay 0.01
        :sustain 1
        :release 0.42]))

                                        ; --- ;

    (let ;saw chords and leads
        [sawchords-rhythm (wrap-at [[]  (E 2 8)]
                                   (wrap-at [0 0 0 0 1 1
                                             1 1 1 1 1 1] section))
         sawleads-rhythm (wrap-at [[]  (E 3 4)]
                                  (wrap-at [0 0 0 0 0 0
                                            0 0 0 1 1 1] section))]
      (play-chord-rhythm-for-tick
       sawchords-rhythm
       4
       tick
       clock
       super-saw
       (wrap-at [(concat (xpose-notes chord 75)
                         (xpose-notes chord 75))
                 ] tick)
       :supersaw
       [:outbus sidechain-carrierbus
        :amp (db->amp -20)
        :width 2.5
        :clamp 0.5])

      (play-chord-rhythm-for-tick
       sawleads-rhythm
       2
       tick
       clock
       super-saw
       [(wrap-at (concat (xpose-notes chord 65)
                         (xpose-notes chord 65)) tick)]
       :supersaw2
       [:outbus sidechain-carrierbus
        :amp (db->amp -16)
        :width 2.5
        :clamp 0.5])

      (play-chord-rhythm-for-tick
       sawleads-rhythm
       2
       tick
       clock
       super-saw
       [(wrap-at (concat (xpose-notes chord 65)
                         (xpose-notes chord 65)) (+ 1 tick))]
       :supersaw3
       [:outbus sidechain-carrierbus
        :amp (db->amp -14)
        :width 4
        :clamp 0.5]))

                                        ; --- ;

    (let ;chimes
        [lowchimesr (wrap-at [[]  (E 4 8)]
                             (wrap-at [1 1 1 1 1 1
                                       0 0 0 0 0 1] section))
         highchimesr (wrap-at [[] (E 4 8)]
                              (wrap-at [0 0 1 1 0 0
                                        0 0 0 0 0 0] section))]
      (play-rhythm-for-tick
       lowchimesr
       4
       tick
       clock
       chimes
       [:amp (db->amp -15)
        :outbus sidechain-carrierbus
        :freq (midi->hz (wrap-at (xpose-notes chord 75) tick))])

      (play-rhythm-for-tick
       highchimesr
       2
       tick
       clock
       chimes
       [:amp (db->amp -15)
        :outbus sidechain-carrierbus
        :freq (fn [] (midi->hz (wrap-at (xpose-notes chord 90)
                                       (+ 2 (e-set :chimesindex
                                                   (+ 1 (e-get :chimesindex 0)))))))]))

                                        ; --- ;

    (comment ;breaks
      (play-rhythm-for-tick
       (E 11 16)
       4
       tick
       clock
       ctl
       [ploop-grain
        :buf (wrap-at vocals (wrap-at (clump-shuffle
                                       (+ 1 (mod tick 4))
                                       [0 3 1 4 2]
                                       [3 4 10 11 16]) tick))
        :offset (fn [] (do (e-set :break-seq (+ 1 (e-get :break-seq -1)))
                          (/ (wrap-at (clump-shuffle
                                       (+ 1 (mod tick 5))
                                       [2 1 0 1 2 3 4]
                                       [0 1 2 3 4 5 6 7 0 1 2 0 1 2 3 4]) (e-get :break-seq 0)) 8)))
        :t-trig 1
        :dur (/ bpm 120)
        :outbus sidechain-carrierbus
        :amp (db->amp 0)]))

    ))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
