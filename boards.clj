(load-file "./algolib.clj")

(def breakbeats (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
(def house (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/house")))
(def openhats (load-sample-dir "/sounds/openhats"))

(def bpm (* 90 1))
(def clk (metronome bpm))

(do
  (definst sidechain [carrierbus 0 modbus 0 outbus 0]
    (let [carrier (in carrierbus 2)
          ;carrier (compander carrier carrier (db->amp -30) 1 0.2 0.001 0.03)
          ;carrier (* (db->amp 20) carrier)
          carrier (free-verb carrier 0.2 0.99 0.2)
          mod (in modbus 2)
          output (compander carrier mod (db->amp -40) 1 0.65 0.005 0.03)
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

(comment)
(do (kill ploop-grain)
    (ploop-grain
     (wrap-at breakbeats 0)
     1 1 0 0.5))

(wrap-at (envelope [0 1 0] [1 1]) 3)

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index tick 8))
        section (section-index tick 32)]

    (comment)
    (let ;percussion
        [
         kick-rhythm (E 2 8)
         snare-rhythm (shr (E 1 8) 4)
         hats-rhythm (E 4 8)
         hats-volume (section-index tick 128 1)
         hats-speed (- 1 (Math/pow (section-index tick 384 1) 1))
         ]

      (play-chord-rhythm-for-tick
       (E 1 4)
       4
       tick
       clock
       super-saw
       (xpose-notes [(first chord)] 24)
       :bass
       [:outbus sidechain-carrierbus
        :release 1
        :cutoff (+ 0.025 (- 1 (section-index tick 64 1)))
        :amp (db->amp 6)
        :width (+ 0.5 (section-index tick 32 3))])

      (comment)
      (cmap (fn [note]
              (play-rhythm-for-tick
               (E 4 4)
               2
               tick
               clock
               prog-res
               [:freq (+ (midi->hz note) (- (rand 10) 5))
                :amp (db->amp -17)
                :decay (+ 2 (section-index tick 64 4))
                :clamp (- 1 (section-index tick 128 1))
                :res 0.2
                :outbus sidechain-carrierbus]))
            (xpose-notes chord (+ 40 (section-index tick 256 30))))

      (comment)
      (play-rhythm-for-tick
       (E 3 8)
       4
       tick
       clock
       noisy-pulse-sweep
       [:freq (fn []
                (midi->hz (xpose-note (wrap-at chord
                                               (e-set :noisyPulseSweep
                                                      (+ 1 (e-get :noisyPulseSweep 0)))) 70)))
        :amp (* (db->amp -15) (if (< 0.75 (section-index tick 128 1)) 1 0))
        :outbus sidechain-carrierbus])

      (play-rhythm-for-tick
       kick-rhythm
       4
       tick
       clock
       psamp
       [:buf (wrap-at house 0)
        :outbus sidechain-modbus
        :amp (db->amp -6)])

      (play-rhythm-for-tick
       snare-rhythm
       4
       tick
       clock
       psamp
       [:buf (wrap-at house 1)
        :amp (db->amp 8)
        :outbus sidechain-carrierbus])

      (play-rhythm-for-tick
       hats-rhythm
       2
       tick
       clock
       psamp
       [:buf (wrap-at openhats 0)
        :amp (* hats-volume (db->amp -12.5))
        :rate (* hats-speed 1)
        :outbus sidechain-carrierbus])

      (play-rhythm-for-tick
       hats-rhythm
       4
       tick
       clock
       psamp
       [:buf (wrap-at openhats 0)
        :amp (* hats-volume (db->amp -11))
        :rate (* hats-speed 0.7)
        :outbus sidechain-carrierbus])

      (play-rhythm-for-tick
       hats-rhythm
       4
       tick
       clock
       psamp
       [:buf (wrap-at openhats 0)
        :amp (* hats-volume (db->amp -13))
        :rate (* hats-speed 0.3)
        :outbus sidechain-carrierbus])

      (comment
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
          :clamp 0.5]))

    )

    (comment)

    (play-rhythm-for-tick
     (E 11 16)
     4
     tick
     clock
     ctl
     [ploop-grain
      :buf (wrap-at breakbeats (wrap-at (clump-shuffle
                                         (+ 1 (mod tick 4))
                                         [0 3 1 4 2]
                                         [3 4 10 11 16]) tick))
      :offset (fn [] (do (e-set :break-seq (+ 1 (e-get :break-seq -1)))
                        (/ (wrap-at (clump-shuffle
                                     (+ 1 (mod tick 5))
                                     [2 1 0 1 2 3 4]
                                     [0 1 2 3 4 5 6 7 0 1 2 0 1 2 3 4]) (e-get :break-seq 0)) 8)))
      :t-trig 1
      :dur (* (/ bpm 120) (ienv [0 0 0.125 1 1] [1 1 1 1] (section-index tick 64 4) 10))
      :outbus sidechain-carrierbus
      :rate (ienv [0 0 0.9 1 1] [1 1 1 1] (section-index tick 64 4) 10)
      :amp (ienv [0 0 0.9 1 1] [1 1 1 1] (section-index tick 64 4) 10)])

    ))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
