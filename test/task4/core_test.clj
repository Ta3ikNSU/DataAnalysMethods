(ns task4.core-test
  [:use task4.api_dnf]
  [:use task4.api_logic]
  (:require [clojure.test :refer :all]))

(deftest tests
  (testing
    (is (=
          (dnf (IMPLICATION
                 (VARIABLE :x)
                 (VARIABLE :y)
                 ))
          (OR (NOT (VARIABLE :x)) (VARIABLE :y))
          ))
    )

  (testing
    (is (=
          (dnf (IMPLICATION
                 (CONSTANT 1)
                 (VARIABLE :y)
                 ))
          (VARIABLE :y)
          ))
    )

  (testing
    (is (=
          (dnf
            (IMPLICATION
              (CONSTANT 1)
              (IMPLICATION
                (VARIABLE :x)
                (VARIABLE :y)
                )))
          (OR (NOT (VARIABLE :x)) (VARIABLE :y))
          ))
    )
  
  (testing
    (is (=
          (dnf (OR
                 (VARIABLE :x)
                 (OR
                   (VARIABLE :y)
                   (VARIABLE :z)
                   )))
          (OR (VARIABLE :x) (VARIABLE :y) (VARIABLE :z))
          ))
    )

  (testing
    (is (=
          (dnf (NOT
                 (OR
                   (IMPLICATION
                     (VARIABLE :x) (VARIABLE :y)
                     )
                   (NOT
                     (IMPLICATION
                       (VARIABLE :y)
                       (VARIABLE :z)
                       )))))
          (OR (AND (VARIABLE :x) (NOT (VARIABLE :y))) (AND (VARIABLE :x) (NOT (VARIABLE :y)) (VARIABLE :z)))
          ))
    )

  (testing
    (is (=
          (dnf (OR
                 (IMPLICATION
                   (VARIABLE :x)
                   (VARIABLE :y)
                   )
                 (IMPLICATION
                   (VARIABLE :x)
                   (VARIABLE :z)
                   )
                 ))
          (OR (NOT (VARIABLE :x)) (VARIABLE :y) (VARIABLE :z))
          ))
    )
  )
