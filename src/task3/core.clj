(ns task3.core)
; ----------------------------------------------------------------------------

; 3.1
; https://stackoverflow.com/questions/27467800/how-to-check-whether-a-number-is-fibonacci-number-in-clojure
(defn integer-sqrt [n]
  (let [n (biginteger n)]
    (loop [a BigInteger/ONE
           b (-> n (.shiftRight 5) (.add (biginteger 8)))]
      (if (>= (.compareTo b a) 0)
        (let [mid (-> a (.add b) (.shiftRight 1))]
          (if (pos? (-> mid (.multiply mid) (.compareTo n)))
            (recur a (.subtract mid BigInteger/ONE))
            (recur (.add mid BigInteger/ONE) b)))
        (dec a)))))

(defn perfect-square? [n]
  (let [x (integer-sqrt n)]
    (= (*' x x) n)))

(defn is-a-fib? [x]
  "Check whether x is a fibonacci number.
   Algorithm: test whether 5x^2+4 or 5x^2-4 is a perfect square."
  (Thread/sleep 1)                                         ; Эмулируем тяжёлую нагрузку
  (let [a (+' (*' (*' x x) 5) 4)                            ; 5x^2+4
        b (-' (*' (*' x x) 5) 4)]                           ; 5x^2-4
    (or (perfect-square? a)
        (perfect-square? b))))

(defn split-col [step ost col]
  (let
    [part (map first
               (iterate
                 (fn [[fst [snd thd]]]
                   (if (> ost thd)
                     [(take (inc step) snd) [(drop (inc step) snd) (inc thd)]]
                     [(take step snd) [(drop step snd) (inc thd)]])
                   )
                 [col [col 0]]))]
    (fn [thread_number] (nth part thread_number))))

(defn thread-filter [cond? col thread-count]
  (let [size (count col),
        step (quot size thread-count),
        ost (mod size thread-count)]
    (->>
      (range 1 (inc thread-count))
      (map #(future (doall (filter cond? ((split-col step ost col) %)))))
      (doall)
      (mapcat deref))))

(defn lazy-filter [cond? col part-n thread-n]
  (lazy-seq (concat (thread-filter cond? (take part-n col) thread-n)
                    (when (seq col)
                      (lazy-filter cond? (drop part-n col) part-n thread-n)))))

(defn -main [& args]
  (time (->>
          (filter is-a-fib? (range 1000))
          (doall)
          ))
  (time (->>
          (thread-filter is-a-fib? (range 1000) 1)
          (doall)
          ))
  (time (->>
          (thread-filter is-a-fib? (range 1000) 2)
          (doall)
          ))
  (time (->>
          (thread-filter is-a-fib? (range 1000) 4)
          (doall)
          ))
  (time (->>
          (thread-filter is-a-fib? (range 1000) 10)
          (doall)
          ))

  (time (->>
          (filter is-a-fib? (range 1000))
          (doall)
          ))
  (time (->>
          (lazy-filter is-a-fib? (range 1000) 10 1)
          (doall)
          ))
  (time (->>
          (lazy-filter is-a-fib? (range 1000) 10 2)
          (doall)
          ))
  (time (->>
          (lazy-filter is-a-fib? (range 1000) 10 4)
          (doall)
          ))
  (time (->>
          (lazy-filter is-a-fib? (range 1000) 10 10)
          (doall)
          ))
  )

