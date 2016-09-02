(use 'overtone.live)
(use 'overtone.helpers.file)
(use '[clojure.string :as str])

(defn cmap
  [func list]
  (if (empty? list)
    '()
    (cons
     (func (first list))
     (cmap func (rest list)))))

(defn split-seq [s]
  "Extract a tail of same elements: [1 1 0 0 0] -> [[1 1] [0 0 0]]"
  (let [l (last s)]
    (split-with #(not= l %) s)))

(defn recombine
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c & more]
   (let [s (concat [a b c] more)
         [head tail] (split-seq s)
         recombined (map concat head tail)
         r-len (count recombined)]
     (if (empty? head)  ;; even pattern
       s
       (apply recombine (concat
                         recombined
                         (drop r-len (drop-last r-len s))))))))

(defn E [k n]
  (let [seed (concat (repeat k [1]) (repeat (- n k) [0]))]
        (flatten (apply recombine seed))))

(defn shl [seq times]
  (if (or (empty? seq) (< times 1))
    seq
    (shl (concat (rest seq) [(first seq)]) (- times 1))))

(defn shr [seq times]
  (if (or (empty? seq) (< times 1))
    seq
    (shr (concat [(last seq)] (drop-last seq)) (- times 1))))

(defn rhythm-to-beats
  ([rhythm length] (rhythm-to-beats rhythm length 0 (count rhythm)))
  ([rhythm length index rlen]
   (if (== (count rhythm) 0) []
       (if (== (first rhythm) 0)
         (rhythm-to-beats (rest rhythm) length (+ index 1) rlen)
         (cons (* (/ index rlen) length) (rhythm-to-beats (rest rhythm) length (+ index 1) rlen))))))

(defn beats-for-tick [seq tick]
  (if (= (count seq) 0)
    []
    (if (and (>= (first seq) tick) (< (first seq) (+ tick 1)))
      (cons (- (first seq) tick) (beats-for-tick (rest seq) tick))
      (beats-for-tick (rest seq) tick))))

(defn rhythm-beats-for-tick [rhythm numbeats tick]
  (beats-for-tick (rhythm-to-beats rhythm numbeats) (mod tick numbeats)))

(defn play-beats [beats clock instrument params]
  (if (not= (count beats) 0)
    (do (at (clock (+ (first beats) (clock))) (apply instrument params))
        (play-beats (rest beats) clock instrument params))))

(defn play-rhythm-for-tick [rhythm numbeats tick clock instrument params]
  (play-beats (rhythm-beats-for-tick rhythm numbeats tick) clock instrument params))

(def bpm 165)
(def clk (metronome bpm))

(comment
  (def chds
    [
     (chord :c4 :major)
     (chord :g4 :major)
     (chord :a4 :minor)
     (chord :f4 :minor)
     ]))

(def chds
  [
   (chord :c4 :major)
   (chord :b4 :sus4)
   (chord :g4 :major)
   (chord :a4 :minor)
   (chord :f4 :minor)
   (chord :e4 :dim)
   ])

(defn cycleChds
  []
  (def chds (concat (rest chds) [(first chds)])))

(def TR-909 "/Users/eric/Documents/Samples/Sample Packs/drum machine samples/Roland TR-909/TR-909/")

(defn load-sample-dir
  ([dir]
   (load-sample-dir (ls-file-paths dir) "wav"))
  ([filepaths extension]
   (if (empty? filepaths)
     []
     (if (= (file-extension (first filepaths)) extension)
       (cons (load-sample (first filepaths)) (load-sample-dir (rest filepaths) extension))
       (load-sample-dir (rest filepaths) extension)))))

(def breaks (load-sample-dir "/sounds/breaks"))
(count breaks)
(def bd909 (sample (str TR-909 "BASSDRUM/BDRUM1.WAV")))
(def sn909 (sample (str TR-909 "SNARES/SNARE1.WAV")))
(def cp909 (sample (str TR-909 "SNARES/CLAP1.WAV")))
(def hh909 (sample (str TR-909 "CYMBALS/HHCLOSE1.WAV")))

(def bd909 (load-sample (str TR-909 "BASSDRUM/BDRUM1.WAV")))
(def sn909 (load-sample (str TR-909 "SNARES/SNARE1.WAV")))
(def cp909 (load-sample (str TR-909 "SNARES/CLAP1.WAV")))
(def hh909 (load-sample (str TR-909 "CYMBALS/HHCLOSE1.WAV")))
(def apache (load-sample "/Users/eric/Documents/Samples/Breaks/78881_OaSyntax_160_apache.wav"))

(definst psamp
  [buf 0 amp 1 rate 1 sr 1 offset 0 outbus 0]
  (out outbus
       (* (decimator (play-buf 1 buf rate :start-pos offset :action FREE) (* 44100 sr)) [amp amp])))

(definst noise
  [dur 1 outbus 0 amp 0.5]
  (let [env (env-gen
             (envelope
              [0 1 0]
              [0.01 dur])
             :action FREE)
        env (pow env 5)]
    (out outbus (* (hpf [(white-noise) (white-noise)] (* env 13000)) env amp))))

(definst buzz
  [freq 440 amp 0.6 dur 1 outbus 0]
  (let [env (env-gen
             (envelope
              [0,1,0]
              [0.01,dur])
             :action FREE)
        saw [(saw (- freq 0.05)) (saw (+ 0.05 freq))]]
    (out outbus (* amp (* saw env)))))

(definst hats
  [amp 1 outbus 0]
  (let [env (env-gen
             (envelope
              [0,0.1,0]
              [0.005, 0.05])
             :action FREE)
        noise (white-noise)]
    (out outbus (* (* noise env) [amp amp]))))

(definst kick
  [amp 1 outbus 0]
  (out outbus
       (let [env (env-gen
                  (envelope
                   [0,1,0]
                   [0.005, 0.15])
                  :action FREE)
             sin (sin-osc
                  (* 400 (* env env env env env)))]
         (* (* sin env) [amp amp]))))


(defn notes-to-intervals [notes]
  (if (not-empty notes)
    (cons
     (mod (first notes) 12)
     (notes-to-intervals (rest notes)))
    []))

(defn shift-notes [notes shiftby]
  (if (not-empty notes)
    (cons
     (+ (first notes) shiftby)
     (shift-notes (rest notes) shiftby))))

(defn clip-notes [notes min max]
  (if (not-empty notes)
    (let [note (first notes)]
      (if (and (>= note min) (<= note max))
        (cons note (clip-notes (rest notes) min max))
        (clip-notes (rest notes) min max)))))

(defn notes-field-helper [notes min max]
  (if (not-empty notes)
    (let [good-notes (clip-notes notes min max)]
      (concat good-notes (notes-field-helper (shift-notes good-notes 12) min max)))))

(defn notes-field [notes min max] (clip-notes (notes-field-helper (notes-to-intervals notes) 0 max) min max))

(defn xpose-note [note floor]
  (let [lowest-note (mod note 12)]
    (letfn [(helper [n f] (if (>= n f) n (helper (+ n 12) f)))] (helper lowest-note floor))))

(defn xpose-notes [notes floor]
  (if (not-empty notes)
    (cons (xpose-note (first notes) floor) (xpose-notes (rest notes) floor))
    []))

(defn wrap-at [seq index]
  (if (not-empty seq)
    (nth seq (mod index (count seq)))
    []))

(definst ploop
  [buf 0 amp 1 rate 1 offset 0 t-trig [1 :tr] outbus 0]
  (out
   outbus
   (let
       [breaks
        (play-buf
         1
         buf
         (* rate (buf-rate-scale buf))
         t-trig
         (* offset (buf-frames buf)) 1)]
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
(def break (ploop apache))

(definst glitch
  [ringbuf 0 bus 0 retrigmix 0 t-retrig [1 :tr] retrigspeed 1 retrigdur 1]
  (let [index (phasor:ar 0 (buf-rate-scale ringbuf) 0 (buf-frames ringbuf))
        index (* index (- 1 retrigmix))
        write (buf-wr (in bus 2) ringbuf index 1)
        retrigindex (phasor:ar
                     t-retrig
                     (* retrigspeed (buf-rate-scale ringbuf))
                     0
                     (* retrigdur retrigspeed (sample-rate)))
        bufframeslatch (latch:ar index t-retrig)
        retrigindex (+ retrigindex bufframeslatch)
        retrigindex (* retrigindex retrigmix)
        index (+ index retrigindex)
        ;index (wrap:ar index 0 (buf-frames ringbuf))
        read (buf-rd 2 ringbuf (- index 200) 1 1)]
    (out 0 (* 0.1 (atan (* 5 read))))))
(def glitchbus (audio-bus 2))
(def glitchinst (glitch (buffer (* 1 44100) 2) glitchbus 0 0 1 1))

(definst fx
  [inbus 0 outbus 0]
  (out outbus (rlpf (in inbus 2) 16000)))
(def fxbus (audio-bus 2))
(def fxinst (fx fxbus 0))

(defn get-song-section [tick] (/ tick 64))

(defn play-beat [clock tick]
  (do
    (comment)
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
    (comment)
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
    (comment)
    (play-rhythm-for-tick
     (E 1 8)
     64
     tick
     clock
     noise
     [:amp 0.1 :dur 15 :outbus glitchbus])
    (comment)
    (play-rhythm-for-tick
     (concat
      (E 3 8)
      (E 4 8)
      (E 3 8)
      (E 5 8))
     16 tick clock psamp [bd909 0.23 0.9 :outbus (wrap-at [fxbus glitchbus] tick)])
    (comment)
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
    (comment)
    (play-rhythm-for-tick
     (concat
      (E (wrap-at [0 11] (get-song-section tick)) 20))
     10 tick clock buzz [(+ (rand 3) (midi->hz (xpose-note (first (first chds)) 30))) 0.25 :outbus glitchbus])
    (play-rhythm-for-tick
     (concat
      (E 11 20))
     10 tick clock buzz [(+ (rand 6) (midi->hz (xpose-note (second (first chds)) 58))) 0.07 :outbus fxbus])
    (play-rhythm-for-tick
     (concat
      (E (wrap-at [0 5] (/ (get-song-section tick) 2)) 8))
     4
     tick
     clock
     buzz
     [(+ (rand 10) (midi->hz (wrap-at (notes-field (first chds) 70 87) tick))) 0.05 :outbus fxbus])
    (play-rhythm-for-tick
     (concat
      (E 5 8))
     16 tick clock cycleChds [])))

(defn play
  [clock tick]
  (let
      [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (def tick (+ tick 1))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
