(load-file "./algolib.clj")

(def bassdrums (load-sample-dir "/sounds/RY30/BASSDRUMS"))
(def cymbals (load-sample-dir "/sounds/RY30/CYMBALS"))
(def fx (load-sample-dir "/sounds/RY30/FX"))
(def misc (load-sample-dir "/sounds/RY30/MISC"))
(def snares (load-sample-dir "/sounds/RY30/SNARES"))
(def toms (load-sample-dir "/sounds/RY30/TOMS"))
(def breaks (load-sample-dir "/sounds/breaks"))

(def bpm (* 130 1))
(def clk (metronome bpm))

(do
  (definst sidechain [carrierbus 0 modbus 0 bassbus 0 outbus 0]
    (let [carrier (in carrierbus 2)
          carrier (compander carrier carrier (db->amp -30) 1 0.2 0.001 0.03)
          carrier (* (db->amp 20) carrier)
          carrier (free-verb carrier 0.08 0.99 0.9)
          carrier (+ carrier (free-verb (in bassbus 2) 0.1 0.8 0.5))
          mod (in modbus 2)
          output (compander carrier mod (db->amp -40) 1 0.65 0.005 0.03)
          output (+ output mod)]
      (out outbus output)))
  (kill sidechain)
  (def sidechain-carrierbus (audio-bus 2))
  (def sidechain-modbus (audio-bus 2))
  (def sidechain-bassbus (audio-bus 2))
  (def sidechain-fx (sidechain sidechain-carrierbus
                               sidechain-modbus
                               sidechain-bassbus
                               0)))

(def chords
  [
   (chord :c4 :sus2)
   (chord :c4 :major)
   (chord :c4 :sus4)
   (chord :c4 :minor)
   ])

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index (* 2 tick) (* 12 2)))
        section (section-index tick 32)
        rhythm (clump-shuffle
                (wrap-at [11 2 23 7 11 3 17 5 15] (section-index tick 32 9))
                [0 4 1 5 2 6 3 4 9 7 10 8 11]
                (E-structure (E (* 5 7) (* 7 7)) [32 16 8 4 2 1]))
        rhythm (E-structure (E (* 3 12) (* 4 12)) [26 14 8 4 2 1])
        kick (sub-pattern   [0 0 0 0 0 0 1 1 1] rhythm)
        snare (sub-pattern  [0 0 0 0 0 1 0 0 0] rhythm)
        cymb (sub-pattern   [0 1 0 1 0 0 0 0 0] rhythm)
        rim (sub-pattern    [0 0 0 0 0 0 1 0 0] rhythm)
        block (sub-pattern  [0 0 0 0 0 1 0 0 0] rhythm)
        click (sub-pattern  [0 1 0 0 0 0 0 0 0] rhythm)
        pop (sub-pattern    [0 0 1 0 0 0 0 0 0] rhythm)
        pbass (sub-pattern  [0 0 0 1 1 1 0 1 0] rhythm)
        pbassp (sub-pattern [0 0 0 0 0 1 0 0 0] rhythm)
        stab (sub-pattern   [0 0 0 0 0 0 0 0 1] rhythm)
        ]

    (comment)
    (play-rhythm-for-tick
     pbass 8 tick clock port-res
     [:freq (midi->hz (wrap-at (xpose-notes chord 26)
                               (e-set :low-bass (+ 11 (e-get :low-bass 0)))))
      :amp (db->amp -1)
      :clamp 0.013
      :res 0.15
      :decay 1.8
      :attack 0.0025
      :exp 15
      :outbus sidechain-bassbus])
    (play-rhythm-for-tick
     pbass 8 tick clock port-res
     [:freq (* 4.995 (midi->hz (wrap-at (xpose-notes chord 38)
                                        (e-set :high-bass (+ 13 (e-get :high-bass 0))))))
      :amp (db->amp -2)
      :clamp 0.05
      :res 0.05
      :decay 0.3
      :attack 0.005
      :exp 20
      :outbus sidechain-bassbus])

    (comment)
    (play-rhythm-for-tick
     kick 8 tick clock psamp
     [:buf (wrap-at bassdrums 0)
      :rate 1
      :outbus sidechain-modbus])
    (play-rhythm-for-tick
     snare 8 tick clock psamp
     [:buf (wrap-at snares 13)
      :amp (db->amp -8)
      :rate 2
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     rim 8 tick clock psamp
     [:buf (wrap-at misc 9)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     cymb 8 tick clock psamp
     [:buf (wrap-at cymbals 3)
      :amp (db->amp -25)
      :rate 1.4
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     block 8 tick clock psamp
     [:buf (wrap-at misc 10)
      :amp (db->amp -7)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     click 8 tick clock psamp
     [:buf (wrap-at misc 16)
      :rate (fn [] (wrap-at [2 1 1.5 2.5 1.75]  (e-set :click-rate (+ 1 (e-get :click-rate 0)))))
      :amp (db->amp -40)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     pop 8 tick clock psamp
     [:buf (wrap-at misc 17)
      :rate (fn [] (wrap-at [1 0.25 2 4 0.5 1 2 3 0.5 1 2]  (e-set :pop-rate (+ 1 (e-get :pop-rate 0)))))
      :amp (db->amp -25)
      :outbus sidechain-carrierbus])
    ))

(play clk 0 play-beat)
