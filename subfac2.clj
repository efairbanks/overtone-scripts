(load-file "./algolib.clj")

(defn list-globals [] (keys (ns-publics *ns*)))

;(recording-start "~/Desktop/foo.wav")
;(recording-stop)
;(println (list-globals))
;(doc notes-field)

                                        ; --------------------- ;
                                        ; --- INITIAL SETUP --- ;
                                        ; --------------------- ;

(def bpm (* 115 1))
(def clk (metronome bpm))

(def housedrums (load-sample-dir "/sounds/house"))
(def openhats (load-sample-dir "/sounds/openhats"))
;(def factory (load-sample-dir "/sounds/factory_115"))
;(def submarine (load-sample-dir "/sounds/submarine_115"))
;(def subfactory (load-sample-dir "/sounds/subfactory_rereresample_230"))

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

(defn break-seq
  [seq divs index]
  (/ (wrap-at seq (mod index divs)) divs))

(do
  ;(kill melody-fx)
  (definst melody-fx [inbus 64 sidechainbus 96 outbus 0]
    (let [sidechain (in sidechainbus 2)
          wet (in inbus 2)
          wet (compander wet sidechain (db->amp -30) 1 0.03 0.005 0.07)
          wet (free-verb wet 0.3 0.95 0.8)
          wet (compander wet sidechain (db->amp -25) 1 0.5 0.005 0.07)
          wet (* wet (db->amp 2))
          wet (+ (* wet (db->amp 5)) sidechain)]
      (out outbus wet)))
  (def melody-bus (audio-bus 2))
  (def sidechain-bus (audio-bus 2))
  (melody-fx melody-bus sidechain-bus))

(def chords
  [
   (chord :c4 :major)
   (chord :g4 :major)
   (chord :f4 :major)
   (chord :f4 :minor)
   ])

(defn section-index
  ([tick length-in-ticks]
   (/ tick length-in-ticks))
  ([tick length-in-ticks count-to]
   (* count-to (/ (mod tick length-in-ticks) length-in-ticks))))

                                        ; ------------------ ;
                                        ; --- SONG START --- ;
                                        ; ------------------ ;

(do
  (if (not= nil (resolve 'melody-fx)) (kill melody-fx))
  (definst melody-fx [inbus 64 sidechainbus 96 outbus 0]
    (let [sidechain (in sidechainbus 2)
          wet (in inbus 2)
          wet (compander wet (lpf sidechain 150) (db->amp -20) 1 0.03 0.001 0.15)
          wet (free-verb wet 0.5 0.95 0.8)
          ;wet (compander wet sidechain (db->amp -25) 1 0.5 0.005 0.07)
          wet (+ (* wet (db->amp 5)) sidechain)]
      (out outbus wet)))
  (def melody-bus (audio-bus 2))
  (def sidechain-bus (audio-bus 2))
  (melody-fx melody-bus glitchbus))

(defn play-beat [clock tick]
  (if true;(> tick 128)
    (do (kill ploop)
        (ctl glitchinst :retrigmix 0))
    (do
      (if (= tick 0) (def break (ploop (wrap-at factory 0))))

                                        ; ---------------------- ;
                                        ; --- MELODY SECTION --- ;
                                        ; ---------------------- ;
      (comment
        (let [rhythm (wrap-at [(E 7 11) (E 11 18) (E 13 20)] (section-index tick 64))
              length (wrap-at [11 18 20] (section-index tick 64))
              clamp (* 0.2 (section-index tick 256 1))
              amp -16
              amp (wrap-at [(db->amp amp)] (section-index tick 128))
              chord-offset (mod (* 1 (Math/floor (/ tick 40))) 12)
              chord (shift-notes (wrap-at chords (/ tick 6)) chord-offset)]

          (play-rhythm-for-tick
           rhythm
           length
           tick
           clock
           port-res
           [:freq (midi->hz (wrap-at (shl (notes-field chord 70 90) 1) tick))
            :amp amp
            :attack 0.04
            :decay 1.7
            :clamp clamp
            :outbus melody-bus])
          (play-rhythm-for-tick
           (E 13 16)
           8
           tick
           clock
           ctl
           [port-res
            :freq (midi->hz (wrap-at (shl (notes-field chord 55 90) 0) tick))
            :amp amp
            :attack 0.04
            :decay 3.7
            :clamp clamp
            :outbus melody-bus])

          (cmap
           (fn [freq]
             (play-rhythm-for-tick
              (E 5 16)
              32
              tick
              clock
              detune
              [:freq freq
               :amp (db->amp -21)
               :outbus melody-bus]))
           (cmap midi->hz (notes-field chord 45 75)))))

                                        ; ------------------------ ;
                                        ; --- HELPER FUNCTIONS --- ;
                                        ; ------------------------ ;

      (defn sin-lfo [index length low high]
        (+ low (* (- high low)
                  (/ (+ 1 (Math/sin (/ (* 6.2832 (mod index length)) length))) 2))))

      (defn cos-lfo [index length low high]
        (+ low (* (- high low)
                  (/ (+ 1 (Math/cos (/ (* 6.2832 (mod index length)) length))) 2))))

                                        ; ---------------------- ;
                                        ; --- RHYTHM SECTION --- ;
                                        ; ---------------------- ;
      (comment
        (let [dummy 1]
          (play-rhythm-for-tick
           (wrap-at [(E 5 16)
                     (E 8 16)
                     (E 11 16)
                     (E 7 16)
                     (E 9 16)
                     (E 4 16)] (/ tick 12))
           (wrap-at [12 8 6 8 8] (/ tick 6))
           tick
           clock
           psamp
           [:buf (wrap-at housedrums 0)
            :amp (db->amp -8)
            :outbus glitchbus])

          (play-rhythm-for-tick
           (wrap-at [(shl (E 5 8) 0)] (/ tick 12))
           (wrap-at [1 2 4 2 8] (/ tick 1))
           tick
           clock
           psamp
           [:buf hh909
            :amp (db->amp (sin-lfo tick 8 -30 -25))
            :outbus glitchbus
            :rate (wrap-at [0.75 2 0.25 4 0.5] (/ tick 6))])

          (play-rhythm-for-tick
           (wrap-at [(shl (E 1 2) 1)] (/ tick 12))
           (wrap-at [8] (/ tick 6))
           tick
           clock
           psamp
           [:buf (wrap-at housedrums 1)
            :amp (db->amp -2.5)
            :outbus glitchbus])))

                                        ; ------------------------- ;
                                        ; --- BREAKBEAT SECTION --- ;
                                        ; ------------------------- ;

      (play-rhythm-for-tick
       (wrap-at [(E 4 16)
                 (E 5 12)] (/ tick 64))
       (wrap-at [4 6] (/ tick 64))
       tick
       clock
       ctl
       [break
        :buf (wrap-at (wrap-at [subfactory] (* tick 13))
                      (wrap-at (shl [4 5 6 7 2 3 0 1]
                                    (+ (* tick 6)
                                       (* tick 5)))
                               (mod tick 8)))
        :offset (break-seq [(rand-int 8)] 8 tick)
        :t-trig 1
        :rate 0.5
        :amp 1
        :outbus glitchbus])

      (comment
        (play-rhythm-for-tick
         (wrap-at [(E 4 16)
                   (E 5 12)] (/ tick 64))
         (wrap-at [4 6] (/ tick 64))
         tick
         clock
         ctl
         [break
          :buf (wrap-at (wrap-at [submarine, factory] (/ tick 1)) (wrap-at [0 4 2 3 1 3 2] (mod tick 5)))
          :offset (break-seq [(rand-int 8)] 8 tick)
          :t-trig 1
          :rate 1.5
          :amp 1
          :outbus glitchbus]))

                                        ; ---------------------- ;
                                        ; --- GLITCH SECTION --- ;
                                        ; ---------------------- ;

      (play-rhythm-for-tick
       (concat
        (E 13 40))
       20
       tick
       clock
       ctl
       [glitchinst
        :retrigmix (wrap-at (shl (E 2 8 ) 2) tick)
        :t-retrig 0
        :retrigdur (/ (beat-ms (wrap-at [0.5 0.25 0.125] tick) bpm) 1000)
        :retrigspeed (wrap-at [1 0.5 1 -1 1.5 1 -0.5] tick)])

      )))

(defn play
  [clock tick]
  (let
      [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (def tick (+ tick 1))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

                                        ;(recording-start "~/Desktop/subfactory2.wav")
(play clk 0)
