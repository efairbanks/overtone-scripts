(load-file "./algolib.clj")

(defn list-globals [] (keys (ns-publics *ns*)))

(println (list-globals))
;(doc notes-field)

(def bpm 165)
(def clk (metronome bpm))

(def chords
  [
   (chord :a4 :major)
   (chord :f4 :minor)
   (chord :g4 :minor)
   (chord :c4 :minor7)
   ])

(defn play-beat [clock tick]
  (do
    (comment)
    (cmap
     (fn [freq]
       (play-rhythm-for-tick
        (E 2 8)
        4
        tick
        clock
        detune
        [:freq freq
         :amp 0.027]))
     (cmap midi->hz (notes-field (wrap-at chords (/ tick 16)) 45 85)))
    (comment)
    (play-rhythm-for-tick
     (E 4 8)
     4
     tick
     clock
     buzz
     [:freq (midi->hz (wrap-at (notes-field (wrap-at chords (/ tick 16)) 30 50) tick))
      :amp 0.04])
    (play-rhythm-for-tick
     (E 7 20)
     5
     tick
     clock
     buzz
     [:freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 16)) 60 82) 1) tick))
      :amp 0.02])
    (play-rhythm-for-tick
     (E 7 10)
     5
     tick
     clock
     buzz
     [:freq (midi->hz (wrap-at (shl (notes-field (wrap-at chords (/ tick 16)) 70 95) 1) tick))
      :amp 0.01])
    (comment)
    (play-rhythm-for-tick
     (E 5 12)
     6
     tick
     clock
     psamp
     [:buf bd909 :amp 0.12])
    (play-rhythm-for-tick
     (E 8 8)
     4
     tick
     clock
     psamp
     [:buf hh909 :amp 0.05])
    (comment)
    (play-rhythm-for-tick
     (shr (E 2 8) 2)
     4
     tick
     clock
     psamp
     [:buf sn909 :amp 0.13])
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
