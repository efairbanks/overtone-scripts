(use 'overtone.live)
(load-file "./algolib.clj")

(def PORT 8000)

                                        ; start a server and create a client to talk with it
(def server (osc-server PORT))
(def client (osc-client "localhost" PORT))

                                        ; Register a handler function for the /test OSC address
                                        ; The handler takes a message map with the following keys:
                                        ;   [:src-host, :src-port, :path, :type-tag, :args]
(osc-handle server "/test" (fn [msg]
                             (do (comment (println msg))
                                 (ctl test
                                      :freq (* 55 (Math/pow 2 (/ (first (msg :args)) 1024)))
                                      :amp (if (< (first (msg :args)) 15) 0 1)))))



(definst test [freq 440 amp 1] (* (saw freq) amp))
(test)
(kill test)
(ctl test :freq 500)
(defsynth)
                                       ;send it some messages
(doseq [val (range 10)]
  (osc-send client "/po" val)
  (Thread/sleep 1000))


                                        ;remove handler
(osc-rm-handler server "/p")

                                        ; stop listening and deallocate resources
(osc-close client)
(osc-close server)


(definst test [freq 440 width 3 amp 1]
  (let [num-voices 20
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (lf-saw:ar freqs (repeatedly num-voices rand)))]
    (out 0 (* amp output))))
