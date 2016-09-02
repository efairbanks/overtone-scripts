(load-file "./algolib.clj")

(def bpm 130)
(def clk (metronome bpm))

(def bassdrums (load-sample-dir "/sounds/RY30/BASSDRUMS"))
(def cymbals (load-sample-dir "/sounds/RY30/CYMBALS"))
(def house (load-sample-dir "/sounds/house"))

(do
  (definst sidechain [carrierbus 0 modbus 0 outbus 0]
    (let [carrier (in carrierbus 2)
          carrier (compander carrier carrier (db->amp -30) 1 0.2 0.001 0.03)
          carrier (* (db->amp 20) carrier)
          carrier (free-verb carrier 0.2 0.99 0.9)
          mod (in modbus 2)
          output (compander carrier mod (db->amp -40) 1 0.65 0.005 0.03)
          output (* output 2)
          output (+ output mod)]
      (out outbus output)))
  (kill sidechain)
  (def sidechain-carrierbus (audio-bus 2))
  (def sidechain-modbus (audio-bus 2))
  (def sidechain-fx (sidechain sidechain-carrierbus
                               sidechain-modbus
                               0)))

(defn A-trans [tick]
    (play-rhythm-for-tick
      (E 1 8) 32 tick clk noise
      [:dur 10
       :amp (db->amp -25)
       :outbus sidechain-modbus]))

(defn A-perc [tick]
     (let []
       (play-rhythm-for-tick
        (E (wrap-at [1 2 3 5 4 3 2] (section-index tick 3)) 8) 4 tick clk psamp
        [:buf (wrap-at bassdrums 0)
         :outbus sidechain-modbus])
       (play-rhythm-for-tick
        (shr (E 2 8) 2) 8 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-modbus
         ])
       (play-rhythm-for-tick
        (shr (E 2 8) 2) 8 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-carrierbus
         ])
       (play-rhythm-for-tick
        (shr (E 4 8) 1) 8 tick clk psamp
        [:buf (wrap-at cymbals 4)
         :rate 1
         :offset 0.3
         :amp (db->amp -11)
         ])
       (comment
         (play-rhythm-for-tick
          (shr (E 8 16) 0) (wrap-at [4 2 1 2] (section-index tick 4)) tick clk psamp
          [:buf (wrap-at cymbals 2)
           :rate 1
           :offset (fn [] (* 0.2 (+ 1 (Math/sin (e-set :hat-offset (+ (* 4 Math/PI 0.125)
                                                                     (e-get :hat-offset 0)))))))
           :amp (db->amp -15)
           ]))))

(defn B-perc [tick]
     (let []
       (play-rhythm-for-tick
        (E (wrap-at [1 2 3 5 4 3 2] (section-index tick 3)) 8) 4 tick clk psamp
        [:buf (wrap-at bassdrums 0)
         :outbus sidechain-modbus])
       (play-rhythm-for-tick
        (shr (E 2 8) 2) 8 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-modbus
         ])
       (play-rhythm-for-tick
        (shr (E 2 8) 2) 8 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-carrierbus
         ])
       (play-rhythm-for-tick
        (shr (E 4 8) 1) 8 tick clk psamp
        [:buf (wrap-at cymbals 4)
         :rate 1
         :offset 0.3
         :amp (db->amp -11)
         ])
       (play-rhythm-for-tick
        (shr (E 8 16) 0) (wrap-at [4 2 1 2] (section-index tick 4)) tick clk psamp
        [:buf (wrap-at cymbals 2)
         :rate 1
         :offset (fn [] (* 0.2 (+ 1 (Math/sin (e-set :hat-offset (+ (* 4 Math/PI 0.125)
                                                                   (e-get :hat-offset 0)))))))
         :amp (db->amp -15)
         ])))

(defn C-perc [tick]
     (let []
       (play-rhythm-for-tick
        (E 4 8) 4 tick clk psamp
        [:buf (wrap-at bassdrums 0)
         :outbus sidechain-modbus])
       (play-rhythm-for-tick
        (shr (E 2 4) 1) 4 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-modbus
         ])
       (play-rhythm-for-tick
        (shr (E 2 4) 1) 4 tick clk psamp
        [:buf (wrap-at house 1)
         :outbus sidechain-carrierbus
         ])
       (play-rhythm-for-tick
        (shr (E 4 8) 1) 8 tick clk psamp
        [:buf (wrap-at cymbals 4)
         :rate 1
         :offset 0.3
         :amp (db->amp -11)
         ])
       (play-rhythm-for-tick
        (shr (E 8 16) 0) (wrap-at [4 2] (section-index tick 1)) tick clk psamp
        [:buf (wrap-at cymbals 2)
         :rate 1
         :offset (fn [] (* 0.2 (+ 1 (Math/sin (e-set :hat-offset (+ (* 4 Math/PI 0.125)
                                                                   (e-get :hat-offset 0)))))))
         :amp (db->amp -15)
         ])))

(defn A-melody [tick chords]
   (let [
         current-chord (wrap-at chords (section-index tick 16))
         freqmod (if (>= (section-index tick 64 64) 60)
                   (+ (/ 4 64) (section-index tick 64 1))
                   1)]

     (play-chord-rhythm-for-tick
      (shl (E 4 8) 0)
      8
      tick
      clk
      super-sin
      (xpose-notes current-chord 80)
      :super-sin
      [:outbus sidechain-carrierbus
       :amp (db->amp -39)])

     (cmap (fn [note]
             (play-rhythm-for-tick
              (shl (E 1 16) 1)
              16
              tick
              clk
              vibstab
              [:outbus sidechain-carrierbus
               :amp (db->amp -18)
               :freq (midi->hz note)]))
           (xpose-notes current-chord 63))

     (let [r-numerator (wrap-at [3 3 3 3 3 3 3 3
                                 3 3 3 3 3 3 3 3
                                 3 3 3 3 3 3 3 3
                                 3 3 3 3 3 3 8 4] (section-index tick 64 32))]
       (play-rhythm-for-tick
        (shl (E r-numerator 8) 2)
        4
        tick
        clk
        prog-res
        [:outbus sidechain-carrierbus
         :amp (db->amp -16)
         :freq (* freqmod (midi->hz (second current-chord)))])
       (play-rhythm-for-tick
        (shl (E r-numerator 8) 2)
        4
        tick
        clk
        prog-res
        [:outbus sidechain-carrierbus
         :amp (db->amp -18)
         :freq (* freqmod 5 (midi->hz (second current-chord)))])))


   (pcrft
     (E 4 8) 8
     super-sqr
     []
     :super-sqr
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)
      :width 0.5
      :cutoff 1])
   )

(defn B-melody [tick chords]
  (let [
        current-chord (wrap-at chords (section-index tick 8))
        player (section-index tick 8 8)]

    ((cond
       (>= player 2)
       (fn [] (play-chord-rhythm-for-tick
              (E 4 8)
              8
              tick
              clk
              super-sin
              (xpose-notes current-chord 73)
              :super-sin
              [:outbus sidechain-carrierbus
               :amp (db->amp -20)]))
       (< player 2)
       (fn [] (play-chord-rhythm-for-tick
              (shl (E 4 8) 0)
              8
              tick
              clk
              super-sin
              []
              :super-sin
              [:outbus sidechain-carrierbus
               :amp (db->amp -20)]))
       :else (fn [])))

    (if (and (>= player 2) (< player 6))
      (do
        (play-rhythm-for-tick
         (shl (E 4 8) 2)
         2
         tick
         clk
         prog-res
         [:outbus sidechain-carrierbus
          :amp (db->amp -16)
          :freq (midi->hz (second current-chord))])
        (play-rhythm-for-tick
         (shl (E 4 8) 2)
         2
         tick
         clk
         prog-res
         [:outbus sidechain-carrierbus
          :amp (db->amp -18)
          :freq (* 5 (midi->hz (second current-chord)))])))

    (if (>= player 6)
      (cmap (fn [note]
              (play-rhythm-for-tick
               (shl (E 4 8) 1)
               8
               tick
               clk
               chimes
               [:outbus sidechain-carrierbus
                :amp (db->amp -18)
                :freq (midi->hz note)]))
            (xpose-notes current-chord 65)))

    (comment
      (if (>= player 6)
        (play-rhythm-for-tick
         (gen-pat [0 1] 4)
         2
         tick
         clk
         wub
         [:outbus sidechain-carrierbus
          :amp (db->amp -41.5)
          :freq (midi->hz (wrap-at (xpose-notes current-chord 30) 2))])))


    (if (< player 2)
      (cmap (fn [note]
              (play-rhythm-for-tick
               [1]
               1
               tick
               clk
               vibstab
               [:outbus sidechain-carrierbus
                :amp (db->amp -16)
                :freq (midi->hz note)]))
            (xpose-notes current-chord 65)))


    (pcrft
     (E 4 8) 8
     super-sqr
     []
     :super-sqr
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)
      :width 0.5
      :cutoff 1])
))

(defn C-melody [tick chords]
  (let [
        current-chord (wrap-at chords (section-index tick 8))
        player (section-index tick 8 8)]

    (pcrft
     (shl (E (wrap-at [13 11] (section-index tick 16)) 24) 0) 6
     port-res-adsr
     [(first current-chord)]
     :port-res
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)
      :decay 4
      :clamp (+ 0.01 (Math/pow (section-index tick 64 1) 8))])

    (prft
     (E 11 16) 2
     ctl
     [port-res-adsr
      :freq (midi->hz (wrap-at
                       (clump-shuffle
                        (+ 1 (mod tick 5)) [0 8 3 6 1 7 4 2 5]
                        (clump-shuffle
                         (+ 1 (mod tick 7)) [6 0 2 5 3 7 1 4]
                           (notes-field current-chord 28 70)))
                       tick))
      :decay (+ 0.1 (* 4 (Math/pow (section-index tick 64 1) 3)))
      :amp (db->amp -18)])

    (let [chord (notes-field current-chord 45 80)]
      (cmap (fn [note]
              (prft
               (shl (E 8 8) 0) 4
               prog-res
               [:freq (midi->hz note)
                :clamp (+ 0.005 (Math/pow (section-index tick 64 1) 5))
                :decay (+ 0.1 (* 8 (Math/pow (section-index tick 64 1) 5)))
                :amp (db->amp -22)
                ]))
            chord))

    (pcrft
     (shl (E 4 8) 0) 8
     super-sin
     []
     :super-sin
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)])
    ))

(defn C2-melody [tick chords]
  (let [
        current-chord (wrap-at chords (section-index tick 8))
        player (section-index tick 8 8)]

    (pcrft
     (shl (E (wrap-at [13 11] (section-index tick 16)) 24) 0) 6
     port-res-adsr
     [(first current-chord)]
     :port-res
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)
      :decay 4
      :clamp (+ 0.01 (Math/pow (- 1 (section-index tick 64 1)) 8))])

    (prft
     (E 11 16) 2
     ctl
     [port-res-adsr
      :freq (midi->hz (wrap-at
                       (clump-shuffle
                        (+ 1 (mod tick 5)) [0 8 3 6 1 7 4 2 5]
                        (clump-shuffle
                         (+ 1 (mod tick 7)) [6 0 2 5 3 7 1 4]
                           (notes-field current-chord 28 70)))
                       tick))
      :decay (+ 0.1 (* 4 (Math/pow (- 1 (section-index tick 64 1)) 2)))
      :amp (db->amp -18)])

    (cmap (fn [note]
            (prft
             (shl (E 8 8) 0) 4
             prog-res
             [:freq (midi->hz note)
              :clamp (+ 0.005 (Math/pow (- 1 (section-index tick 64 1)) 3))
              :decay (+ 0.1 (* 8 (Math/pow (- 1 (section-index tick 64 1)) 3)))
              :amp (db->amp -22)
              ]))
          (notes-field current-chord 45 80))

    (pcrft
     (shl (E 4 8) 0) 8
     super-sin
     []
     :super-sin
     [:outbus sidechain-carrierbus
      :amp (db->amp -20)])
    ))

(defn kick-riser [tick]
  (prft
        (E (* 2 (+ 1 (section-index tick 2 2))) 4) 1 psamp
        [:buf (wrap-at bassdrums 0)
         :attack 0.002
         :amp (db->amp -7)
         :sr (+ 0.25 (section-index tick 4 0.75))
         :rate (wrap-at [0.7 0.8 0.9 1.0] (section-index tick 4 1))
         :offset (fn [] (wrap-at [0 0.025 0.0125] (e-set :kick-riser-offset-index
                                           (+ 1 (e-get :kick-riser-offset-index 0)))))
         :outbus sidechain-carrierbus]))

(defn riser-into-B-melody [tick]
  (let [chords [
                (chord :b1 :major)
                ]
        current-chord (wrap-at chords (section-index tick 64 4))
        player (section-index tick 64 64)
        freqmod (section-index tick 64 1)]

    (if (>= player 63)
      (kick-riser tick))

    (if (>= player 60)
      (do
        (play-rhythm-for-tick
         (shl (E 4 8) 2)
         2
         tick
         clk
         prog-res
         [:outbus sidechain-carrierbus
          :amp (db->amp -16)
          :freq (* freqmod (midi->hz (second current-chord)))])
        (play-rhythm-for-tick
         (shl (E 4 8) 2)
         2
         tick
         clk
         prog-res
         [:outbus sidechain-carrierbus
          :amp (db->amp -18)
          :freq (* freqmod 5 (midi->hz (second current-chord)))])))))

(defn play-beat
 [clock tick]
 (e-set :clock clock)
 (e-set :tick (+ (wrap-at [1 1
                           1 1
                           1 1
                           1 1
                           1 1
                           2 2
                           2 2
                           2 2
                           2 2
                           2 2]
                          (section-index (e-get :tick 0) 32))
                 (e-get :tick 0)))

 (let [tick (e-get :tick 0)
       asc-d-prog [(chord :d2 :minor)
                   (chord :c2 :major)
                   (chord :c#2 :minor)
                   (chord :c#2 :major)]
       asc-a-prog [(chord :c#1 :minor)
                   (chord :c#1 :a)
                   (chord :d1 :major)
                   (chord :d#1 :minor)
                   (chord :d#1 :a)
                   (chord :e1 :major)
                   (chord :f1 :minor)
                   (chord :f1 :a)
                   (chord :f#1 :major)
                   (chord :g1 :minor)
                   (chord :g1 :a)
                   (chord :g#1 :major)
                   (chord :a1 :minor)
                   (chord :a1 :a)
                   (chord :a#1 :major)
                   (chord :b1 :dim)]
       dsc-a-prog [(chord :b1 :dim)]
       asc-c-prog [(chord :c2 :major)
                   (chord :c#2 :minor)
                   (chord :c#2 :major)
                   (chord :d2 :minor)]
       dsc-c-prog [(chord :c2 :major7)
                   (chord :c2 :minor7)
                   (chord :b1 :major7)
                   (chord :b1 :minor7)]]

   (A-trans tick)

   ((wrap-at [(fn [tick])
              (fn [tick])
              B-perc
              A-perc
              B-perc] (section-index tick 64)) tick)

   ((wrap-at [C-melody
              C2-melody
              B-melody
              B-melody
              A-melody] (section-index tick 64)) tick
              (wrap-at [asc-a-prog
                        asc-a-prog
                        (wrap-at [dsc-c-prog asc-c-prog] (section-index tick (* 64 5)))
                        (wrap-at [asc-c-prog dsc-c-prog] (section-index tick (* 64 5)))
                        (wrap-at [dsc-c-prog asc-c-prog] (section-index tick (* 64 5)))]
                       (section-index tick 64)))

   ((wrap-at [(fn [tick])
              riser-into-B-melody
              (fn [tick])
              (fn [tick])
              (fn [tick])
              ] (section-index tick 64)) tick))
 )

(play clk 0 play-beat)
;(stop)

                                        ; * Three different risers
                                        ; * Three different falls
                                        ; * Three variations on each section
                                        ; * Hemiola and potentially complextro bit should have one
                                        ;   more constantly active instrument in higher register
                                        ; * Optional additive elements to complextro and hemiola
                                        ;   parts
                                        ; √ hemiola part should have AAAB/BBBA pattern instead of
                                        ;   AAAA
                                        ; √ Hemiola and complextro parts should trade ascending /
                                        ;   descending progressions
