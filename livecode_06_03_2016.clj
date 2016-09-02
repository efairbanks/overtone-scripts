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
          output (+ output mod)]
      (out outbus output)))
  (kill sidechain)
  (def sidechain-carrierbus (audio-bus 2))
  (def sidechain-modbus (audio-bus 2))
  (def sidechain-fx (sidechain sidechain-carrierbus
                               sidechain-modbus
                               0)))

(defn play-beat
 [clock tick]
 (let [chd (wrap-at [
                       (chord :c4 :minor)
                       (chord :b4 :major)
                       (chord :e4 :minor)
                       (chord :a4 :minor)
                       (chord :c4 :minor)
                       (chord :b4 :major)
                       (chord :e4 :minor)
                       (chord :f4 :major)
                       ] (section-index tick 4))]

   (comment)
   (play-rhythm-for-tick
    (E 1 8) 32 tick clk noise
    [:dur 10
     :amp (db->amp -20)
     :outbus sidechain-modbus])

   (play-rhythm-for-tick
    (E 4 8) 4 tick clk psamp
    [:buf (wrap-at bassdrums 0)
     :outbus sidechain-modbus])

   (play-rhythm-for-tick
    (shr (E 2 8) 2) 4 tick clk psamp
    [:buf (wrap-at house 1)
     ])

   (comment)
   (play-rhythm-for-tick
    (shr (E 4 8) 1) 4 tick clk psamp
    [:buf (wrap-at cymbals 4)
     :amp (db->amp -11)
     ])
   (play-rhythm-for-tick
    (shr (E 8 16) 3) 4 tick clk psamp
    [:buf (wrap-at cymbals 2)
     :amp (db->amp -13)
     ])

   (comment)
   (play-rhythm-for-tick
    (shr (E 3 8) 1)
    4
    tick
    clk
    prog-res
    [:freq (midi->hz (first (xpose-notes chd 30)))
     :amp (db->amp -14)
     :clamp (+ 0.01 (section-index tick 64 0.1))
     :outbus sidechain-carrierbus])

                                        ; ok that's it, bye all!



   (cmap
    (fn [note]
      (play-rhythm-for-tick
       (E 5 16)
       8
       tick
       clk
       prog-res
       [:freq (midi->hz note)
        :amp (db->amp -20)
        :outbus sidechain-carrierbus]))
    (xpose-notes (rest chd) 55))

   (play-chord-rhythm-for-tick
    (E 4 8)
    4
    tick
    clock
    super-sin
    (xpose-notes chd (+ 60 (section-index tick 64 15)))
    :pads
    [:amp (db->amp -16)
     :outbus sidechain-carrierbus
     ]))
 )

(play clk 0 play-beat)
