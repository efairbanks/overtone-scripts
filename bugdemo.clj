(use 'overtone.live)

(def bpm (* 180 1))
(def clk (metronome bpm))

(defn cmap
  [func list]
  (if (empty? list)
    '()
    (cons
     (func (first list))
     (cmap func (rest list)))))

(definst super-saw [freq 440 width 3 gate 1]
  (let [num-voices 80
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (lf-saw:ar freqs (repeatedly num-voices rand)))
        output (* output (env-gen (adsr 0.1 0.1 1 0.5 1) :gate gate :action FREE))
        output (* output 5)
        output [output output]]
    (out 0 output)))

(defn play-beat
  [clock tick]
  (do
    (ctl super-saw :gate 0)
    (super-saw (* 1000 (+ 1 (rand))) 10)))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
