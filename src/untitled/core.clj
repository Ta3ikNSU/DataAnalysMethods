(ns untitled.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
; ----------------------------------------------------------------------------
(defn gen-permutations-1-1 [n alphabet]
  (if (zero? n)
    [""]
    (for [symbol alphabet
          prev (gen-permutations-1-1 (dec n) (remove #{symbol} alphabet))
          :when (not= symbol (first prev))]
      (str symbol prev))
    )
  )

(defn main-1-1
  [& args]
  (let [n (last args)]
    (let [alphabet (butlast args)]
      (if (> n (count alphabet))
        (throw (Exception. "n больше размера алфавита")))
      (println n)
      (println alphabet)
      (println (gen-permutations-1-1 n alphabet))
      )
    )
  )
; ----------------------------------------------------------------------------