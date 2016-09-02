(load-file "./algolib.clj")

(def vocals (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
(def house (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/house")))
(def openhats (load-sample-dir "/sounds/openhats"))

(def bpm (* 160 1))
(def clk (metronome bpm))

(definst super-saw [freq 440 amp 1 width 3 gate 1 outbus 0]
  (let [num-voices 40
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (lf-saw:ar freqs (repeatedly num-voices rand)))
        output (* output (env-gen (adsr 0.05 0.1 0.95 1 1) gate :action FREE))
        output (* output amp)
        output [output output]]
    (out outbus output)))

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
  (kill sidechain)
  (definst sidechain [carrierbus 0 modbus 0 outbus 0]
    (let [carrier (in carrierbus 2)
          ;carrier (compander carrier carrier (db->amp -30) 1 0.2 0.001 0.03)
          carrier (* (db->amp 16) carrier)
          mod (in modbus 2)
          output (compander carrier mod (db->amp -30) 1 0.5 0.001 0.03)
          output (+ output mod)]
      (out outbus output)))
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
  (let [chord (wrap-at chords (section-index tick 8))]

    (comment)
    (play-rhythm-for-tick
     (E 2 8)
     2
     tick
     clock
     psamp
     [:buf (wrap-at house 0)
      :outbus sidechain-modbus])

    (play-rhythm-for-tick
     (shr (E 2 8) 4)
     2
     tick
     clock
     psamp
     [:buf (wrap-at house 1)])

    (comment)
    (play-rhythm-for-tick
     (shr (E 4 8) 0)
     2
     tick
     clock
     psamp
     [:buf (wrap-at openhats 0)
      :offset 0
      :amp (db->amp -20)
      :outbus sidechain-carrierbus])

    (comment
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

    (play-chord-rhythm-for-tick
     (E 4 16)
     4
     tick
     clock
     super-saw
     (xpose-notes [(second chord)] 31)
     :bass
     [:outbus sidechain-carrierbus
      :amp (db->amp -6)
      :width 3])

    (comment)
    (let [speed (wrap-at [2 1 2 2 1 1 2] tick)]
      (play-chord-rhythm-for-tick
       (E (* 8 speed) 16)
       4
       tick
       clock
       wub
       (xpose-notes [(second chord)] (wrap-at [40 20 30] tick))
       :wub
       [:outbus sidechain-carrierbus
        :amp (db->amp -25)
        :scaler (wrap-at [16.01 4.03 8.2 12.05 7] tick)]))

    (play-chord-rhythm-for-tick
     (E 4 8)
     4
     tick
     clock
     super-saw
     (wrap-at [(concat (xpose-notes chord 75)
                       (xpose-notes chord 75))
               ] tick)
     :supersaw
     [:outbus sidechain-carrierbus
      :amp (db->amp -25)
      :width 2.5
      :clamp 0.5])

    (play-chord-rhythm-for-tick
     (E 3 4)
     2
     tick
     clock
     super-saw
     [(wrap-at (concat (xpose-notes chord 65)
                       (xpose-notes chord 65)) tick)]
     :supersaw2
     [:outbus sidechain-carrierbus
      :amp (db->amp -13)
      :width 2.5
      :clamp 0.5])

    (play-chord-rhythm-for-tick
     (E 3 4)
     2
     tick
     clock
     super-saw
     [(wrap-at (concat (xpose-notes chord 65)
                       (xpose-notes chord 65)) (+ 1 tick))]
     :supersaw3
     [:outbus sidechain-carrierbus
      :amp (db->amp -13)
      :width 4
      :clamp 0.5])

    (play-rhythm-for-tick
     (E 4 8)
     4
     tick
     clock
     chimes
     [:freq (midi->hz (wrap-at (xpose-notes chord 80) tick))])

    (play-rhythm-for-tick
     (E 4 8)
     2
     tick
     clock
     chimes
     [:freq (midi->hz (wrap-at (xpose-notes chord 90) (+ 2 tick)))])

    ))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
