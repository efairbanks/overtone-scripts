(load-file "./algolib.clj")

(def breakbeats (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
(def house (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/house")))
(def openhats (load-sample-dir "/sounds/openhats"))

(def bpm (* 85 1))
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
   (chord :c4 :sus2)
   (chord :c4 :major)
   (chord :c4 :sus4)
   (chord :c4 :minor)
   ])

(do (kill ploop-grain)
    (ploop-grain
     (wrap-at breakbeats 0)
     1 1 0 0.5))

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index tick 8))
        section (section-index tick 32)
        port-res-rhythm (E 3 8)]

    (play-chord-rhythm-for-tick
     (E 4 8)
     4
     tick
     clock
     super-saw
     (concat [(second (xpose-notes chord 30))]
             (xpose-notes chord 60)
             (xpose-notes chord 72))
     :super-saw-bass
     [:outbus sidechain-carrierbus
      :width 3
      :amp (db->amp -2)])

    (play-rhythm-for-tick
     (E 11 16)
     8
     tick
     clock
     ctl
     [ploop-grain
      :buf (wrap-at breakbeats 9)
      :offset (fn [] (do (e-set :break-seq (+ 1 (e-get :break-seq -1)))
                        (/ (wrap-at (clump-shuffle
                                     (+ 1 (mod tick 5))
                                     [2 1 0 1 2 3 4]
                                     [0 1 2 3 4 5 6 7 0 1 2 0 1 2 3 4]) (e-get :break-seq 0)) 8)))
      :t-trig 1
      :dur (/ bpm 240)
      :outbus sidechain-carrierbus
      :rate 1
      :amp (db->amp -3)])

    (cmap (fn [note]
            (play-rhythm-for-tick
             port-res-rhythm
             2
             tick
             clock
             port-res
             [:freq (midi->hz note)
              :decay 7
              :clamp (ienv [0 1] [1] (section-index tick 8 1) 3)
              :outbus sidechain-carrierbus])) chord)

    (play-rhythm-for-tick
     (E 4 16)
     4
     tick
     clock
     psamp
     [:buf (wrap-at house 0)
      :outbus sidechain-modbus
      :amp (db->amp -7.5)])

    (play-rhythm-for-tick
     (shr (E 1 2) 1)
     1
     tick
     clock
     psamp
     [:buf (wrap-at house 1)
      :outbus sidechain-modbus
      :amp (db->amp -4.5)])

    (play-rhythm-for-tick
     (E 0 8)
     2
     tick
     clock
     psamp
     [:buf (wrap-at openhats 1)
      :outbus sidechain-modbus
      :amp (db->amp -4)])

    ))

(play clk 0 play-beat)
