(load-file "./algolib.clj")

(def bpm (* 90 1))
(def clk (metronome bpm))

(def chords
  [
   (chord :f4 :minor)
   (chord :c4 :minor)
   (chord :g4 :major)
   (chord :d4 :major)
   (chord :a4 :major)
   (chord :e4 :major)
   (chord :b4 :minor)
   (chord :f4 :minor)
   ])

(definst port-res [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0]
  (let [env (pow (env-gen (envelope [0 1 0] [attack decay]) :action FREE) exp)
        freq (lag freq 0.1)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(do (kill ploop-grain)
    (ploop-grain
     (wrap-at vocals 0)
     1 1 0 0.5))

(defn play-beat [clock tick]
  (let [chord (wrap-at chords (section-index tick 8))]

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
      :amp (db->amp -5)])

    (comment)
    (cmap (fn [note]
            (play-rhythm-for-tick
             (E 9 14)
             7
             tick
             clock
             port-res
             [:freq (midi->hz note)
              :amp (db->amp -29)
              :decay 10])) (xpose-notes chord 68))

    (play-rhythm-for-tick
     (E 13 20)
     10
     tick
     clock
     port-res
     [:freq (midi->hz (first (xpose-notes chord 31)))
      :amp (db->amp -17)
      :decay 5])

    (play-rhythm-for-tick
     (E 31 44)
     11
     tick
     clock
     port-res
     [:amp (db->amp -18)
      :decay 2
      :freq (fn []
              (do (e-set :pitch-index (+ 1 (e-get :pitch-index -1)))
                  (midi->hz (wrap-at (xpose-notes chord 55) (e-get :pitch-index 0)))))])

    (play-rhythm-for-tick
     (E 41 52)
     13
     tick
     clock
     port-res
     [:amp (db->amp -20)
      :decay 3
      :freq (fn []
              (do (e-set :pitch-index (+ 1 (e-get :pitch-index -1)))
                  (midi->hz (wrap-at (xpose-notes (shl chord 1) 60) (e-get :pitch-index 0)))))])



    ))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)

                                        ; ---- ;
                                        ; ---- ;
                                        ; ---- ;

(comment
  (do
    (def vocals (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
    (definst growl [buf 0 len 0.1]
      (let [env (env-gen (envelope [0 1 1 0] [0.001 (- (* 2 len) 0.002) 0.001]))
            tone (warp1 1 buf (mouse-x 0 1) 1 (/ 1.0 10) -1)
            output (* 1 tone env)]
        (out 0 output)))
    (growl :buf (wrap-at vocals 0))))

(def vocals (cmap (fn [buffer] (buffer-mix-to-mono buffer)) (load-sample-dir "/sounds/breaks")))
(definst ploop-grain
  [buf 0 amp 1 rate 1 offset 0 dur 1 t-trig [1 :tr] outbus 0]
  (out
   outbus
   (let
       [index (/ (phasor:ar
                  t-trig
                  (* dur (/ (buf-frames buf) (sample-rate)))
                  0
                  (buf-frames buf)
                  (* offset (buf-frames buf)))
                 (buf-frames buf))
        breaks
        (warp1
         1
         buf
         index
         (* rate (buf-rate-scale buf))
         (/ 1.0 20.0)
         -1
         4
         )]
     (* [amp amp]
        (db->amp 13)
        (compander
         breaks
         breaks
         (db->amp -32)
         1
         1/3
         0.003
         0.03)))))
