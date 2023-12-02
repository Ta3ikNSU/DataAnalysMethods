(ns task1.core)

; ----------------------------------------------------------------------------
; #1.1
(defn append-char-if-not-last
  [word symbol]
  (if (= (str (last word)) symbol)
    nil
    (str word symbol)))

(defn concat-symbol-from-alphabet-to-word                   ; в конец слова (базы) допихиваем все слова из алфавита
  [base alphabet result]
  (if (empty? alphabet)
    result
    (let [new-word (append-char-if-not-last base (first alphabet))
          new-seq (rest alphabet)]
      (if (nil? new-word)
        (concat-symbol-from-alphabet-to-word base new-seq result)
        (concat-symbol-from-alphabet-to-word base new-seq (into result (list new-word)))))))

(defn concat-alphabet-to-all-words
  [result alphabet current-words]
  (if (empty? result)
    current-words
    (concat-alphabet-to-all-words (rest result) alphabet (into current-words (concat-symbol-from-alphabet-to-word (first result) alphabet []))))) ; перебираем все элементы из текущей свалки слов

(defn generate-sequences
  [result alphabet length]
  (if (empty? alphabet)
    `()
    (if (empty? result)
      (generate-sequences alphabet alphabet length)         ; первые буквы для всех слов
      (if (= (count (first result)) length)                 ; если первое слово имеет нужную длину -> все слова тоже
        result
        (generate-sequences (concat-alphabet-to-all-words result alphabet []) alphabet length)))))

; ----------------------------------------------------------------------------
; #1.2
(defn concat-symbol-from-alphabet-to-word-recur
  [base alphabet result]
  (if (empty? alphabet)
    result
    (let [new-word (append-char-if-not-last base (first alphabet))
          new-seq (rest alphabet)]
      (if (nil? new-word)
        (recur base new-seq result)
        (recur base new-seq (into result (list new-word)))))))

(defn concat-alphabet-to-all-words-recur
  [result alphabet current-word]
  (if (empty? result)
    current-word
    (recur (rest result) alphabet (into current-word (concat-symbol-from-alphabet-to-word-recur (first result) alphabet [])))))

(defn generate-sequences-recur
  [result alphabet length]
  (if (empty? alphabet)
    `()
    (if (empty? result)
      (recur alphabet alphabet length)
      (if (= (count (first result)) length)
        result
        (recur (concat-alphabet-to-all-words-recur result alphabet []) alphabet length)))))

; ----------------------------------------------------------------------------
; #1.3
(defn my-map
  [f coll]
  (reverse (reduce (fn [acc x] (conj acc (f x))) '() coll)))

(defn my-filter
  [pred coll]
  (reverse (reduce (fn [acc x] (if (pred x) (conj acc x) acc)) '() coll)))
; ----------------------------------------------------------------------------
; #1.4
(defn concat-symbol-from-alphabet-to-word-recur-reduce-enabled
  [base alphabet]
  (my-filter some? (my-map #(append-char-if-not-last base %1) alphabet)))

(defn concat-alphabet-to-all-words-recur-reduce-enabled
  [result alphabet]
  (reverse (reduce
             #(into %1 (concat-symbol-from-alphabet-to-word-recur-reduce-enabled %2 alphabet))
             `()
             result)))

(defn generate-sequences-recur-reduce-enabled
  [alphabet length]
  (if (empty? alphabet)
    `()
    (reduce
      (fn [acc _]
        (if (empty? acc)
          alphabet
          (concat-alphabet-to-all-words-recur-reduce-enabled acc alphabet)))
      []
      (range 0 length))))

(defn -main [& args]
  (println (generate-sequences-recur-reduce-enabled ["a", "b", "c"] 3)))