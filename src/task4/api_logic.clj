(ns task4.api_logic)

(defn CONSTANT
  [value]
  {:pre [(or (= value 0) (= value 1))]}
  (list ::const value))

(defn isCONSTANT?
  [expression]
  (= (first expression) ::const))

(defn getConstantValue
  [expression]
  (second expression))

(defn VARIABLE
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn isVARIABLE?
  [expression]
  (= (first expression) ::var))

(defn getVariableName
  [var]
  (name (second var)))

(defn isSameVariables?
  [var1 var2]
  (and
    (isVARIABLE? var1)
    (isVARIABLE? var2)
    (= (getVariableName var1)
       (getVariableName var2))))

(defn NOT
  [expression]
  (list ::not expression))

(defn isNOT?
  [expression]
  (= (first expression) ::not))

(defn OR
  [expression & rest]
  (if (empty? rest)
    expression
    (cons ::or (cons expression rest))))

(defn isOR?
  [expression]
  (= (first expression) ::or))

(defn AND
  [expression & rest]
  (if (empty? rest)
    expression
    (cons ::and (cons expression rest))))

(defn isAND?
  [expression]
  (= (first expression) ::and))

(defn IMPLICATION
  [expression1 expression2]
  (list ::impl expression1 expression2))

(defn isIMPLICATION?
  [expression]
  (= (first expression) ::impl))

(defn get-type
  [expression]
  (keyword (first expression)))

(defn same-type?
  [expression1 expression2]
  (= (first expression1) (first expression2)))

(defn args
  [expression]
  (rest expression))

(defn first-arg
  [expression]
  (first (rest expression)))

(defn second-arg
  [expression]
  (second (rest expression)))

(defn combine-same-args
  [expression]
  (if (or (isCONSTANT? expression) (isVARIABLE? expression))
    (list expression)
    (->> (args expression)
         (mapcat #(if (same-type? expression %)
                    (combine-same-args %)
                    (list %))))))

(defn buildStringFromExpression
  [expression]
  (cond
    (isCONSTANT? expression) (getConstantValue expression)
    (isVARIABLE? expression) (getVariableName expression)
    (isNOT? expression) (str "(" (str "!" (buildStringFromExpression (first (args expression)))) ")")
    (isOR? expression) (str "(" (reduce #(str %1 " | " %2)
                                  (buildStringFromExpression (first (args expression)))
                                  (map buildStringFromExpression (rest (args expression)))) ")")
    (isAND? expression) (str "(" (reduce #(str %1 " & " %2)
                                   (buildStringFromExpression (first (args expression)))
                                   (map buildStringFromExpression (rest (args expression)))) ")")
    (isIMPLICATION? expression) (str "(" (str (buildStringFromExpression (first (args expression))) " -> " (buildStringFromExpression (second (args expression)))) ")")))