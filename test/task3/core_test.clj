(ns task3.core-test
  (:require [clojure.test :refer :all]
            [task3.core :refer :all :as ta3ik]))



(deftest tests
  (testing "Testing thread-filter"
    (is (=
          (filter is-a-fib? (range 100))
          (thread-filter is-a-fib? (range 100) 1)))
    (is (=
          (filter is-a-fib? (range 100))
          (thread-filter is-a-fib? (range 100) 2)))
    (is (=
          (filter is-a-fib? (range 100))
          (thread-filter is-a-fib? (range 100) 3)))
    (is (=
          (filter is-a-fib? (range 100))
          (thread-filter is-a-fib? (range 100) 10)))))

(deftest tests
  (testing "Testing p-filter-inf"
    (is (=
          (filter even? (range 100))
          (lazy-filter even? (range 100) 5 1)))
    (is (=
          (filter even? (range 100))
          (lazy-filter even? (range 100) 5 2)))
    (is (=
          (filter even? (range 100))
          (lazy-filter even? (range 100) 5 3)))
    (is (=
          (filter even? (range 100))
          (lazy-filter even? (range 100) 5 4)))
    ))