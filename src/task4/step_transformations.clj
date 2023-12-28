(ns task4.step_transformations
  (:use task4.api_logic))

; x -> y = !x | y
(defn transformation-implication
  [expression]
  (cond
    (isOR? expression) (apply OR (map #(transformation-implication %) (args expression)))
    (isAND? expression) (apply AND (map #(transformation-implication %) (args expression)))
    (isNOT? expression) (NOT (transformation-implication (first-arg expression)))
    (isIMPLICATION? expression) (OR (NOT (transformation-implication (first-arg expression))) (transformation-implication (second-arg expression)))
    :else expression))

;   !(x | y) = !x & !y;
;   !(x & y) = !x | !y
;   !(!x) = x
;   !0 = 1;
;   !1 = 0
;   Отрицание
(defn transformation-negation
  [expression]
  (cond
    (isNOT? expression) (let [sub-expression (first-arg expression)]
                        (cond
                          (isOR? sub-expression) (apply AND (map #(transformation-negation (NOT %)) (args sub-expression)))
                          (isAND? sub-expression) (apply OR (map #(transformation-negation (NOT %)) (args sub-expression)))
                          (isNOT? sub-expression) (transformation-negation (first-arg sub-expression))
                          (isCONSTANT? sub-expression) (if (= sub-expression (CONSTANT 0)) (CONSTANT 1) (CONSTANT 0))
                          :else expression))
    (isOR? expression) (apply OR (map #(transformation-negation %) (args expression)))
    (isAND? expression) (apply AND (map #(transformation-negation %) (args expression)))
    :else expression))

;   x | (x & y) = x; x & (x | y) = x
;   Закон поглощения
(defn absorption-law
  ([expression]
   (cond
     (isOR? expression) (absorption-law isAND? OR (first-arg expression) (second-arg expression))
     (isAND? expression) (absorption-law isOR? AND (first-arg expression) (second-arg expression))
     :else expression))
  ([inner-op-check outer-op sub-expression1 sub-expression2]
   (cond
     (inner-op-check sub-expression1) (if (or (= sub-expression2 (first-arg sub-expression1)) (= sub-expression2 (second-arg sub-expression1)))
                                  (absorption-law sub-expression2)
                                  (outer-op (absorption-law sub-expression1) (absorption-law sub-expression2)))
     (inner-op-check sub-expression2) (if (or (= sub-expression1 (first-arg sub-expression2)) (= sub-expression1 (second-arg sub-expression2)))
                                  (absorption-law sub-expression1)
                                  (outer-op (absorption-law sub-expression1) (absorption-law sub-expression2)))
     :else (outer-op (absorption-law sub-expression1) (absorption-law sub-expression2)))))

;   (x | y) & z = (x & z) | (y & z)
;   Распределительный закон
(defn distributive-law
  [expression]
  (cond
    (isAND? expression) (let [sub-expression1 (first-arg expression) sub-expression2 (second-arg expression)]
                        (cond
                          (isOR? sub-expression1) (OR (distributive-law (AND sub-expression2 (first-arg sub-expression1)))
                                                (distributive-law (AND sub-expression2 (second-arg sub-expression1))))
                          (isOR? sub-expression2) (OR (distributive-law (AND sub-expression1 (first-arg sub-expression2)))
                                                (distributive-law (AND sub-expression1 (second-arg sub-expression2))))
                          :else expression))
    (isOR? expression) (apply OR (map #(distributive-law %) (args expression)))
    :else expression))

; x & (y & z) = x & y & z
(defn ^:private decompose
  [expression]
  (if (or (isOR? expression) (isAND? expression))
    (cons (getType expression) (map #(decompose %) (combine-same-args expression)))
    expression))

(^:private decompose(AND (VARIABLE :x) (AND (VARIABLE :y) (VARIABLE :z))))

; x & x = x; x | x = x
; Закон идентпотентности
(defn idempotent-law
  [expression]
  (cond
    (isAND? expression) (apply AND (map #(idempotent-law %) (distinct (args (decompose expression)))))
    (isOR? expression) (apply OR (map #(idempotent-law %) (distinct (args (decompose expression)))))
    (isNOT? expression) (NOT (idempotent-law (first-arg expression)))
    :else expression))

;   x & 0 = 0;
;   x | 1 = 1
;   x & 1 = x;
;   x | 0 = x
;   базовые преобразования
(defn identity-and-domination-laws
  ([expression]
   (let [expression (decompose expression)]
     (cond
       (isAND? expression) (cond
                      (identity-and-domination-laws expression (CONSTANT 0)) (CONSTANT 0)
                      (identity-and-domination-laws expression (CONSTANT 1)) (identity-and-domination-laws (apply AND (filter #(not (= % (CONSTANT 1))) (args expression))))
                      :else (apply AND (map #(identity-and-domination-laws %) (args expression))))
       (isOR? expression) (cond
                      (identity-and-domination-laws expression (CONSTANT 1)) (CONSTANT 1)
                      (identity-and-domination-laws expression (CONSTANT 0)) (identity-and-domination-laws (apply OR (filter #(not (= % (CONSTANT 0))) (args expression))))
                      :else (apply OR (map #(identity-and-domination-laws %) (args expression))))
       :else expression)))
  ([expression const]
   (some #(and (isCONSTANT? %) (= const %)) (args expression))))