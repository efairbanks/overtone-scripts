(load-file "./algolib.clj")

(defn list-globals [] (keys (ns-publics *ns*)))

(println (list-globals))
;(doc notes-field)

(def bpm 165)
(def clk (metronome bpm))

(def break (ploop apache))

                                        ; --- basic progression --- ;

(def chords
  [
   (chord :c4 :major)
   (chord :g4 :major)
   (chord :a4 :minor)
   (chord :f4 :minor)
   ])

                                        ; --- embellished progression --- ;

(def chords
  [
   (chord :c4 :major)
   (chord :b4 :sus4)
   (chord :g4 :major)
   (chord :a4 :minor)
   (chord :f4 :minor)
   (chord :e4 :dim)
   ])

(def chordIndex 0)
(defn cycleChords [] (def chordIndex (+ chordIndex 1)))

(defn play-beat [clock tick]
  (do
                                        ; --- pulse orchestra --- ;
    (cmap
     (fn [freq]
       (play-rhythm-for-tick
        (wrap-at [(E 0 16)
                  (E 0 16)
                  (E 11 16)] (/ (get-song-section tick) 2))
        32
        tick
        clock
        detune
        [:freq freq
         :amp 0.025
         :outbus 0]))
     (cmap midi->hz (notes-field (wrap-at chords chordIndex) 45 95)))

                                        ; --- break beat masher --- ;

    (play-rhythm-for-tick
     (concat
      (E 4 16))
     4
     tick
     clock
     ctl
     [break
      :buf (wrap-at
            breaks
            (wrap-at (wrap-at [[2] [3 4 10 0] [7] [5 8 1]] (get-song-section tick)) (mod (* 7 tick) 11)))
      :rate 1
      :offset (wrap-at [(/ (mod tick 4) 4) (/ (mod (* 11 tick) 8) 8)] (get-song-section tick))
      :t-trig 1
      :amp (* 1 0.78)
      :outbus glitchbus])

                                        ; --- control glitch fx --- ;

    (play-rhythm-for-tick
     (concat
      (E 13 40))
     20
     tick
     clock
     ctl
     [glitchinst
      :retrigmix (wrap-at (E (wrap-at [0 4] (get-song-section tick)) 23) tick)
      :t-retrig 0
      :retrigdur (/ (beat-ms (wrap-at [0.5 1 0.125 0.25 0.75] tick) bpm) 1000)
      :retrigspeed (wrap-at [1 0.5 1 -1 1.5 1 -0.5] tick)])

                                        ; --- noise sweeps --- ;:

    (play-rhythm-for-tick
     (E 1 8)
     64
     tick
     clock
     noise
     [:amp 0.1 :dur 15 :outbus glitchbus])

                                        ; --- wacky TR-909 percussion --- ;

    (play-rhythm-for-tick
     (concat
      (E 3 8)
      (E 4 8)
      (E 3 8)
      (E 5 8))
     16 tick clock psamp [bd909 0.23 0.9 :outbus (wrap-at [fxbus glitchbus] tick)])
    (play-rhythm-for-tick
     (concat
      (E 5 8))
     8 tick clock psamp [hh909 0.1 0.2 0.03 :outbus fxbus])
    (play-rhythm-for-tick
     (concat
      (shr (E 2 20) 8))
     20 tick clock psamp [sn909 0.25 0.35 0.3 :outbus glitchbus])
    (play-rhythm-for-tick
     (concat
      (shr (E 2 20) 8))
     20 tick clock psamp [cp909 0.3 0.75 0.2 :outbus glitchbus])

                                        ; --- detuned sawtooth melodies --- ;

    (play-rhythm-for-tick
     (concat
      (E (wrap-at [0 11] (get-song-section tick)) 20))
     10 tick clock buzz [(+ (rand 3) (midi->hz (xpose-note (first (wrap-at chords chordIndex)) 30))) 0.25 :outbus glitchbus])
    (play-rhythm-for-tick
     (concat
      (E 11 20))
     10 tick clock buzz [(+ (rand 6) (midi->hz (xpose-note (second (wrap-at chords chordIndex)) 58))) 0.07 :outbus fxbus])
    (play-rhythm-for-tick
     (concat
      (E (wrap-at [0 5] (/ (get-song-section tick) 2)) 8))
     4
     tick
     clock
     buzz
     [(+ (rand 10) (midi->hz (wrap-at (notes-field (wrap-at chords chordIndex) 70 87) tick))) 0.05 :outbus fxbus])

                                        ; --- DnB TR-909 Percussion --- ;
    ;(if (= (mod (get-song-section tick) 2) 1))
    (do
      (play-rhythm-for-tick
       (E 5 12)
       6
       tick
       clock
       psamp
       [:buf bd909 :amp 0.25 :outbus glitchbus])
      (play-rhythm-for-tick
       (E 19 24)
       6
       tick
       clock
       psamp
       [:buf hh909 :amp 0.065 :rate 4 :outbus glitchbus])
      (play-rhythm-for-tick
       (shr (E 2 8) 2)
       4
       tick
       clock
       psamp
       [:buf sn909 :amp 0.33 :outbus glitchbus]))

                                        ; --- trigger chord changes --- ;

    (play-rhythm-for-tick
     (concat
      (E 5 8))
     16 tick clock cycleChords [])
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

                                        ; --------------------- ;
                                        ; --- how to record --- ;
                                        ; --------------------- ;

                                        ;(recording-start "~/Desktop/foo.wav")
                                        ;(recording-stop)
