(ns task2.core-test
  (:require [clojure.test :refer :all]
            [task2.core :refer :all :as ta3ik]))
(deftest trapezoid-test
  (testing "Площадь трапеции"
    (is (= (trapezoid (fn [x] x) 0 2) 2.0))
    (is (= (trapezoid (fn [x] (* x x)) 0 1) 0.5))
    (is (= (trapezoid (fn [x] (* x x)) 1 2) 2.5))))

(deftest integral-test
  (testing "Интеграл"
    (is (= (integral (fn [x] x) -10 10 5) 0.0))
    (is (= (integral (fn [x] (* x x)) -10 0 10) 500.0))
    (is (= (integral (fn [x] (* x x)) 0 10 10) 500.0))
    (is (< (/ (integral polynomial -5 5 0.005) 99080.95) (* 0.01 99080.95))) ; погрешность менее процента
    ))

(deftest integral-mem-test
  (testing "Интеграл мемоизированный"
    (is (= (m-integral (fn [x] x) -10 10 5) 0.0))
    (is (= (m-integral (fn [x] (* x x)) -10 0 10) 500.0))
    (is (= (m-integral (fn [x] (* x x)) 0 10 10) 500.0))

    (is (< (/ (m-integral polynomial -5 5 0.005) 99080.95) (* 0.01 99080.95)))
    ))
