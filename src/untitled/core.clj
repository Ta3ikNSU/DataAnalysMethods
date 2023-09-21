(ns untitled.core)

(defn foo1
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

; memorize
; ----------------------------------------------------------------------------
(defn my-map [func coll]
  (reverse
    (reduce
      (fn
        [acc elem]
        (cons (func elem) acc))
      (list)
      coll)))

(defn remove-element-helper [my-vector element-to-remove index]
  (if (= 0 index)
    []
    (if (= (nth my-vector (dec index)) element-to-remove)
      (remove-element-helper my-vector element-to-remove (- index 1))
      (conj (remove-element-helper my-vector element-to-remove (- index 1)) (nth my-vector (dec index)))
      )
    )
  )

(defn remove-element [my-vector element-to-remove]
  (remove-element-helper my-vector element-to-remove (count my-vector)))

(defn generate-string [array n answers current need-length]
  (if (= (count current) need-length)
    (conj answers current)
    (if (= n -1)
      answers
      (let [new-list-without-element (remove-element array (nth array n))]
        (concat
          (generate-string new-list-without-element (- (count new-list-without-element) 1) answers (str current (nth array n)) need-length)
          (generate-string array (- n 1) answers current need-length)
          )
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
        (generate-string alphabet (- (count alphabet) 1) [] "" n)
      )
    )
  )
; ----------------------------------------------------------------------------