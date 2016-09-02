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

(def chords
  [
   (chord :c4 :sus2)
   (chord :c4 :major)
   (chord :c4 :sus4)
   (chord :c4 :minor)
   ])

(comment)
(do (kill ploop-grain)
    (ploop-grain
     (wrap-at breaks 0)
     1 1 0 0.5))

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index (* 2 tick) (* 7 7)))
        section (section-index tick 32)
        rhythm (clump-shuffle
                (wrap-at [11 2 23 7 11 3 17 5 15] (section-index tick 32 9))
                [0 4 1 5 2 6 3 4 9 7 10 8 11]
                (E-structure (E (* 5 7) (* 7 7)) [32 23 17 11 7 4 2]))
        rhythm (E-structure (E (* 5 7) (* 7 7)) [32 23 17 11 7 4 2])
        kick (sub-pattern   [0 0 0 0 0 0 1 0 1] rhythm)
        snare (sub-pattern  [0 0 0 0 0 1 1 0 0] rhythm)
        cymb (sub-pattern   [0 0 0 0 0 0 0 0 0] rhythm)
        rim (sub-pattern    [0 0 0 0 0 1 0 0 0] rhythm)
        block (sub-pattern  [0 0 0 0 1 0 0 0 0] rhythm)
        click (sub-pattern  [0 0 0 1 0 0 0 0 0] rhythm)
        pbass (sub-pattern  [0 0 0 0 0 0 1 0 1] rhythm)
        pbassp (sub-pattern [0 0 0 1 1 1 0 0 0] rhythm)
        stab (sub-pattern   [0 0 0 0 0 0 0 0 1] rhythm)
        ]

    (comment)
    (cmap (fn [note]
            (play-rhythm-for-tick
             stab 7 tick clock prog-res
             [:freq (midi->hz note)
              :amp (db->amp -3)
              :clamp 0.2
              :decay 0.8
              :outbus sidechain-carrierbus]))
          (xpose-notes chord 66))

    (play-chord-rhythm-for-tick
     pbass
     7
     tick
     clock
     super-sin
     (xpose-notes chord 67)
     :supersin
     [:outbus sidechain-carrierbus
      :amp (db->amp -6)])

    (comment)
    (play-rhythm-for-tick
     pbass 7 tick clock port-res
     [:freq (midi->hz (second (xpose-notes chord 25)))
      :outbus sidechain-carrierbus
      :amp (db->amp -2)
      :decay 2])
    (play-rhythm-for-tick
     pbassp 7 tick clock ctl
     [port-res
      :freq (* (midi->hz (first (xpose-notes chord 40)))
               (/ (e-set :pbass (+ 1 (mod (+ 1 (e-get :pbass 0)) 7)))
                  (e-set :pbass2 (+ 1 (mod (+ 1 (e-get :pbass2 0)) 5)))))])

    (cmap (fn [note]
            (play-rhythm-for-tick
             (shl (E 1 7) 3)
             7
             tick
             clock
             noisy-pulse-sweep
             [:freq (midi->hz note)
              :outbus sidechain-carrierbus
              :amp (db->amp -25)]))
          (xpose-notes chord 65))

    (play-rhythm-for-tick
     kick 7 tick clock psamp
     [:buf (wrap-at bassdrums 0)
      :outbus sidechain-modbus])
    (play-rhythm-for-tick
     snare 7 tick clock psamp
     [:buf (wrap-at snares 3)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     cymb 7 tick clock psamp
     [:buf (wrap-at cymbals 4)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     rim 7 tick clock psamp
     [:buf (wrap-at snares 1)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     click 7 tick clock psamp
     [:buf (wrap-at fx 6)
      :outbus sidechain-carrierbus])
    (play-rhythm-for-tick
     block 7 tick clock psamp
     [:buf (wrap-at misc 10)
      :outbus sidechain-carrierbus])


    (comment)
    (play-rhythm-for-tick
     kick
     7
     tick
     clock
     ctl
     [ploop-grain
      :buf (wrap-at breaks 9)
      :offset (fn [] (do (e-set :break-seq (+ 1 (e-get :break-seq -1)))
                        (/ (wrap-at (clump-shuffle
                                     (+ 1 (mod tick 5))
                                     [2 1 0 1 2 3 4]
                                     [0 1 2 3 4 5 6 7 0 1 2 0 1 2 3 4]) (e-get :break-seq 0)) 8)))
      :t-trig 1
      :dur (/ bpm 240)
      :outbus sidechain-carrierbus
      :rate 1
      :amp (db->amp -23)])

    ))

(play clk 0 play-beat)
