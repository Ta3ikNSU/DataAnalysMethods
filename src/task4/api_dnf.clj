(ns task4.api_dnf
  (:use task4.api_logic task4.step_transformations))
(defn dnf
  [expression]
  (->> expression
       (transformation-implication)
       (transformation-negation)
       (distributive-law)
       (absorption-law)
       (idempotent-law)
       (identity-and-domination-laws)
       )
  )