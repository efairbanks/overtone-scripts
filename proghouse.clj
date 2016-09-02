(load-file "./algolib.clj")

(defn list-globals [] (keys (ns-publics *ns*)))

;(recording-start "~/Desktop/foo.wav")
;(recording-stop)
(println (list-globals))
;(doc notes-field)

(def bpm 190)
(def clk (metronome bpm))

(def housedrums (load-sample-dir "/sounds/house"))
(def openhats (load-sample-dir "/sounds/openhats"))

(definst prog-res [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0]
  (let [env (pow (env-gen (envelope [0 1 0] [attack decay]) :action FREE) exp)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(definst port-res [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0]
  (let [env (pow (env-gen (envelope [0 1 0] [attack decay]) :action FREE) exp)
        freq (lag freq 0.1)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(do
  ;(kill melody-fx)
  (definst melody-fx [inbus 64 sidechainbus 96 outbus 0]
    (let [sidechain (in sidechainbus 2)
          wet (in inbus 2)
          wet (compander wet sidechain (db->amp -30) 1 0.03 0.005 0.07)
          wet (free-verb wet 0.3 0.95 0.8)
          wet (compander wet sidechain (db->amp -25) 1 0.5 0.005 0.07)
          wet (+ (* wet (db->amp 5)) sidechain)]
      (out outbus wet)))
  (def melody-bus (audio-bus 2))
  (def sidechain-bus (audio-bus 2))
  (melody-fx melody-bus sidechain-bus))

(def chords
  [
   (chord :c4 :minor)
   (chord :f4 :minor)
   (chord :g4 :minor)
   (chord :c4 :sus4)

   (chord :c4 :minor)
   (chord :d4 :major)
   (chord :e4 :sus2)
   (chord :g4 :major)

   (chord :c4 :major)
   (chord :f4 :minor)
   (chord :b4 :dim)
   (chord :c4 :major)

   (chord :c4 :minor)
   (chord :f4 :sus2)
   (chord :g4 :major)
   (chord :ab4 :major)
   ])

(defn section-index
  ([tick length-in-ticks]
   (/ tick length-in-ticks))
  ([tick length-in-ticks count-to]
   (* count-to (/ (mod tick length-in-ticks) length-in-ticks))))



(defn play-beat [clock tick]
  (do

    (play-rhythm-for-tick
     (E 1 64)
     64
     tick
     clock
     noise
     [:amp (db->amp -30)
      :dur 20
      :outbus melody-bus])

    (comment
      (cmap
       (fn [freq]
         (play-rhythm-for-tick
          (E 1 128)
          128
          tick
          clock
          echo-blip
          [:freq freq
           :outbus melody-bus]))
       (cmap midi->hz (notes-field (wrap-at chords (section-index tick 8)) 70 85))))

    (cmap
     (fn [freq]
       (play-rhythm-for-tick
        (wrap-at [(E 4 4)] (section-index tick 128))
        8
        tick
        clock
        detune
        [:freq freq
         :amp (* (wrap-at [0 (db->amp -23)] (section-index tick (+ 128 64)))
                 (section-index tick 64 1))
         :outbus melody-bus]))
     (cmap midi->hz (notes-field (wrap-at chords (section-index tick 8)) 45 65)))


    (comment)
    (cmap
     (fn [freq]
       (play-rhythm-for-tick
        (wrap-at [(E 4 8)
                  (E 11 16)
                  (shr (E 4 8) 1)
                  (E 9 16)
                  (E 5 8)
                  (E 7 16)] (section-index tick 128))
        8
        tick
        clock
        prog-res
        [:freq freq
         :amp (wrap-at [0
                        0
                        (db->amp -21)
                        (db->amp -21)
                        (db->amp -21)] (section-index tick 128))
         :exp 7
         :decay (+ 1.25 (section-index tick 64 1))
         :clamp (+ 0.03 (* 0.1 (section-index tick 64 1)))
         :outbus melody-bus]))
     (cmap midi->hz (notes-field (wrap-at chords (section-index tick 8)) 45 65)))

    (comment)
    (play-rhythm-for-tick
     (wrap-at [(E 4 8)
               (shr (E 5 8) 2)
               (E 2 8)
               (shr (E 3 8) 4)
               (shr (E 2 8) 2)] (section-index tick 128))
     4
     tick
     clock
     prog-res
     [:freq (midi->hz (wrap-at (notes-field (wrap-at chords (/ tick 8)) 30 50) tick))
      :amp (db->amp -11)
      :clamp (* 0.05 (section-index tick 16 1) (Math/sin (section-index tick 64 (/ 3.14 2))))
      :outbus melody-bus])

    (let [rhythm (wrap-at [(E 7 11) (E 11 16) (E 9 13) (E 13 20)] (section-index tick 64))
          length (wrap-at [11 8 13 10] (section-index tick 64))
          clamp (* 0.2 (section-index tick 256 1))
          amp (wrap-at [0 (db->amp -17)] (section-index tick 128))]
      (play-rhythm-for-tick
       rhythm
       length
       tick
       clock
       prog-res
       [:freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 8)) 55 70) 1) tick))
        :amp amp
        :attack 0.04
        :decay 0.7
        :clamp clamp
        :outbus melody-bus])
      (play-rhythm-for-tick
       rhythm
       length
       tick
       clock
       prog-res
       [:freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 8)) 55 70) 3) tick))
        :amp amp
        :attack 0.04
        :decay 0.7
        :clamp clamp
        :outbus melody-bus]))

    (let [rhythm (wrap-at [(E 7 11) (E 11 18) (E 13 20)] (section-index tick 64))
          length (wrap-at [11 18 20] (section-index tick 64))
          clamp (* 0.2 (section-index tick 256 1))
          amp -17
          amp (wrap-at [0 0 (db->amp amp) 0 (db->amp amp) 0] (section-index tick 128))]
      (play-rhythm-for-tick
       rhythm
       length
       tick
       clock
       port-res
       [:freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 8)) 70 90) 5) tick))
        :amp amp
        :attack 0.04
        :decay 1.7
        :clamp clamp
        :outbus melody-bus])
      (play-rhythm-for-tick
       (E 11 16)
       8
       tick
       clock
       ctl
       [port-res
        :freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 8)) 55 90) 0) tick))
        :amp amp
        :attack 0.04
        :decay 1.7
        :clamp clamp
        :outbus melody-bus]))

    (comment
      (play-rhythm-for-tick
       (wrap-at [(shl (E 3 4) 1) (E 1 2) (E 3 6) (shr (E 1 4) 2) [0 1 1 0] [0] (E 16 16)] 0)
       2
       tick
       clock
       psamp
       [:buf (wrap-at openhats (rand-int 6))
        :amp (db->amp (+ -25 (* 5 (Math/sin (section-index tick 8 6.28)))))
        :rate 0.7
        :outbus melody-bus]))

    (play-rhythm-for-tick
     (E 4 4)
     8
     tick
     clock
     psamp
     [:buf (wrap-at housedrums 0) :amp (db->amp -9.5) :outbus sidechain-bus])

    (play-rhythm-for-tick
     (wrap-at [(shl (E 3 4) 1) (E 1 2) (E 3 6) (shr (E 1 4) 2) [0 1 1 0] [0] (E 16 16)] 0)
     2
     tick
     clock
     psamp
     [:buf hh909 :amp (db->amp (+ -35 (* 5 (Math/sin (section-index tick 8 6.28))))) :rate 0.95])

    (play-rhythm-for-tick
     (shr (E 2 8) 2)
     8
     tick
     clock
     psamp
     [:buf (wrap-at housedrums 2) :amp (db->amp -11)])

    (play-rhythm-for-tick
     (shr (E 2 8) 2)
     8
     tick
     clock
     psamp
     [:buf cp909 :amp (db->amp -13) :outbus melody-bus])
    )
  )

(defn play
    [clock tick]
    (let
        [beat (clock)]
      (do (at (clock beat) (play-beat clock tick))
          (def tick (+ tick 1))
          (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
