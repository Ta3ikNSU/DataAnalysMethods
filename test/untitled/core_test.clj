(ns untitled.core-test
  (:require [clojure.test :refer :all]
            [untitled.core :refer :all :as ta3ik]))

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

