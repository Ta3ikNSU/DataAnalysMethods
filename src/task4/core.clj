(ns task4.core
  (:use [task4.api_dnf]
        [task4.api_logic]))
; ----------------------------------------------------------------------------

; 4.1
; https://www.kontrolnaya-rabota.ru/s/mathlogic
(defn -main []
  (println "x -> y =" (buildStringFromExpression (dnf (IMPLICATION
                                          (VARIABLE :x)
                                          (VARIABLE :y)
                                          ))))
  (println "1 -> y =" (buildStringFromExpression (dnf (IMPLICATION
                                          (CONSTANT 1)
                                          (VARIABLE :y)
                                          ))))
  (println "1 -> (x -> y) =" (buildStringFromExpression (dnf
                                            (IMPLICATION
                                              (CONSTANT 1)
                                              (IMPLICATION
                                                (VARIABLE :x)
                                                (VARIABLE :y)
                                                )))))
  (println "x | y | z =" (buildStringFromExpression (dnf (OR
                                             (VARIABLE :x)
                                             (OR
                                               (VARIABLE :y)
                                               (VARIABLE :z)
                                               )))))
  (println "!((x -> y) | !(y -> z)) =" (buildStringFromExpression (dnf (NOT
                                                           (OR
                                                             (IMPLICATION
                                                               (VARIABLE :x) (VARIABLE :y)
                                                               )
                                                             (NOT
                                                               (IMPLICATION
                                                                 (VARIABLE :y)
                                                                 (VARIABLE :z)
                                                                 )))))))
  (println "((x -> y) | (x -> z)) =" (buildStringFromExpression (dnf (OR
                                                         (IMPLICATION
                                                           (VARIABLE :x)
                                                           (VARIABLE :y)
                                                           )
                                                         (IMPLICATION
                                                           (VARIABLE :x)
                                                           (VARIABLE :z)
                                                           )
                                                         ))))
  )
