(ns task1.core
  (:require [clojure.string :as s]))

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
(defn add-char-to-string [string chars]
  (my-map #(str string %1) (my-filter #(not (s/ends-with? string %1)) chars)))

(defn add-char [chars strings]
  (reduce concat (map #(add-char-to-string %1 chars) strings)))

(defn generate-sequences-recur-reduce-enabled [chars n]
  (cond
    (<= n 0) '()
    (= (count chars) 0) '()
    (and (= (count chars) 1) (> n 1)) '()
    :else (nth (iterate #(add-char chars %1) chars) (dec n))))

(defn -main [& args]
  (println (generate-sequences-recur-reduce-enabled ["a", "b", "c"] 3)))