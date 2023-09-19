(ns untitled.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
; ----------------------------------------------------------------------------
(defn remove-element [my-vector element-to-remove]
  (filter #(not (= % element-to-remove)) my-vector))

(defn generate-string [array n answers current need-length]
  (if (= (count current) need-length)
    (println current)
    (if (= n -1)
      answers
      (let [new-list-without-element (remove-element array (nth array n))]
          (generate-string new-list-without-element (- (count new-list-without-element) 1) answers (str current (nth array n)) need-length)
          (generate-string array (- n 1) answers current need-length)
        )
      )
    )
  )

(defn main-1-1
  [& args]
  (let [n (last args)]
    (let [alphabet (butlast args)]
      (if (> n (count alphabet))
        (throw (Exception. "n больше размера алфавита")))
      (let [answers (generate-string alphabet (- (count alphabet) 1) [] "" n)]
        (doseq [answer answers]
          (println answer))))))
; ----------------------------------------------------------------------------