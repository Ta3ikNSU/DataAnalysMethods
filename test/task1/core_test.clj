(ns task1.core-test
  (:require [clojure.test :refer :all]
            [task1.core :refer :all :as ta3ik]))

(deftest test-append-char-if-not-last
  (testing "append-char-if-not-last should append a character if it's not the last character of the word"
    (is (= (ta3ik/append-char-if-not-last "test" "e") "teste"))
    (is (= (ta3ik/append-char-if-not-last "" "a") "a"))
    (is (= (ta3ik/append-char-if-not-last "a" "a") nil))))

(deftest test-concat-symbol-from-alphabet-to-word
  (testing "concat-symbol-from-alphabet-to-word should concatenate symbols from the alphabet to the base word"
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word "base" ["a" "b" "c"] []) ["basea" "baseb" "basec"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word "word" ["x" "y" "z"] []) ["wordx" "wordy" "wordz"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word "test" ["1" "2" "3"] []) ["test1" "test2" "test3"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word "" ["x" "y" "z"] []) ["x" "y" "z"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word "" [] []) []))))

(deftest test-generate-sequences
  (testing "generate-sequences should generate sequences of specified length using the alphabet"
    (is (= (ta3ik/generate-sequences [] ["a" "b" "c"] 2) ["ab" "ac" "ba" "bc" "ca" "cb"]))
    (is (= (ta3ik/generate-sequences [] ["a" "b" "c"] 3) ["aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc"]))
    (is (= (ta3ik/generate-sequences [] ["1" "2" "3"] 1) ["1" "2" "3"]))))


(deftest test-concat-symbol-from-alphabet-to-word-recur
  (testing "concat-symbol-from-alphabet-to-word-recur should concatenate symbols from the alphabet to the base word"
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur "base" ["a" "b" "c"] []) ["basea" "baseb" "basec"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur "word" ["x" "y" "z"] []) ["wordx" "wordy" "wordz"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur "test" ["1" "2" "3"] []) ["test1" "test2" "test3"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur "" ["x" "y" "z"] []) ["x" "y" "z"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur "" [] []) []))))

(deftest test-generate-sequences-recur
  (testing "generate-sequences-recur should generate sequences of specified length using the alphabet"
    (is (= (ta3ik/generate-sequences-recur [] ["a" "b" "c"] 2) ["ab" "ac" "ba" "bc" "ca" "cb"]))
    (is (= (ta3ik/generate-sequences-recur [] ["a" "b" "c"] 3) ["aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc"]))
    (is (= (ta3ik/generate-sequences-recur [] ["1" "2" "3"] 1) ["1" "2" "3"]))))


(deftest test-my-map
  (testing "my-map should map a function to each element in the collection"
    (is (= (ta3ik/my-map inc [1 2 3 4 5]) '(2 3 4 5 6)))
    (is (= (ta3ik/my-map inc '(1 2 3 4 5)) '(2 3 4 5 6)))
    (is (= (ta3ik/my-map str '(a b c d e)) '("a" "b" "c" "d" "e")))
    (is (= (ta3ik/my-map (fn [x] (* x x)) '(1 2 3 4 5)) '(1 4 9 16 25)))
    (is (= (ta3ik/my-map (fn [x] (str "item" x)) '(1 2 3)) '("item1" "item2" "item3")))
    (is (= (ta3ik/my-map identity '()) '()))))

(deftest test-my-filter
  (testing "my-filter should filter elements in the collection based on a predicate"
    (is (= (ta3ik/my-filter even? '(1 2 3 4 5 6)) '(2 4 6)))
    (is (= (ta3ik/my-filter odd? '(1 2 3 4 5 6)) '(1 3 5)))
    (is (= (ta3ik/my-filter #(> % 2) '(1 2 3 4 5)) '(3 4 5)))
    (is (= (ta3ik/my-filter #(<= % 3) '(1 2 3 4 5)) '(1 2 3)))
    (is (= (ta3ik/my-filter #(= % 0) '()) '()))))


(deftest test-concat-symbol-from-alphabet-to-word-recur
  (testing "concat-symbol-from-alphabet-to-word-recur should concatenate symbols from the alphabet to the base word"
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur-reduce-enabled "base" ["a" "b" "c"]) ["basea" "baseb" "basec"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur-reduce-enabled "word" ["x" "y" "z"]) ["wordx" "wordy" "wordz"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur-reduce-enabled "test" ["1" "2" "3"]) ["test1" "test2" "test3"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur-reduce-enabled "" ["x" "y" "z"]) ["x" "y" "z"]))
    (is (= (ta3ik/concat-symbol-from-alphabet-to-word-recur-reduce-enabled "" []) []))))

(deftest test-generate-sequences-recur
  (testing "generate-sequences-recur should generate sequences of specified length using the alphabet"
    (is (= (ta3ik/generate-sequences-recur-reduce-enabled ["a" "b" "c"] 2) ["ab" "ac" "ba" "bc" "ca" "cb"]))
    (is (= (ta3ik/generate-sequences-recur-reduce-enabled ["a" "b" "c"] 3) ["aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc"]))
    (is (= (ta3ik/generate-sequences-recur-reduce-enabled ["1" "2" "3"] 1) ["1" "2" "3"]))))