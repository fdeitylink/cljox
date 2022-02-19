(ns cljox.interpreter
  (:require [cljox.ast :as ast]
            [cljox.error :as error]
            [cljox.token :as token]))

(defmethod error/fmt ::nonnumeric-unary-operand
  [{::keys [operator right] :as error}]
  (format "operand '%s' to '%s' must be numeric" (pr-str right) (::token/lexeme operator)))

(defmethod error/fmt ::nonnumeric-binary-operands
  [{::keys [operator left right] :as error}]
  (format "operands '%s', '%s' to '%s' must be numeric" (pr-str left) (pr-str right) (::token/lexeme operator)))

(defmethod error/fmt ::invalid-plus-operands
  [{::keys [left right] :as error}]
  (format "operands '%s', '%s' to + must both be numeric or one must be a string" (pr-str left) (pr-str right)))

(defmethod error/fmt ::division-by-zero
  [{::keys [left] :as error}]
  (format "division of '%s' by 0" (pr-str left)))

(defn- numeric-operator
  ([operator f r]
   (if (number? r)
     (f r)
     (error/runtime-error ::nonnumeric-unary-operand operator
                          ::operator operator ::right r)))
  ([operator f l r]
   (if (and (number? l) (number? r))
     (f l r)
     (error/runtime-error ::nonnumeric-binary-operands operator
                          ::operator operator ::left l ::right r))))

(defn- add
  [operator l r]
  (cond
    (and (number? l) (number? r)) (+ l r)
    (or (string? l) (string? r)) (str l r)
    :else (error/runtime-error ::invalid-plus-operands operator
                               ::left l ::right r)))

(defn- divide
  [operator l r]
  (numeric-operator operator
                    (fn [l r]
                      (if (zero? r)
                        (error/runtime-error ::division-by-zero operator ::left l)
                        (/ l r)))
                    l r))

(defmulti ^:private evaluate ::ast/type)

(defmethod evaluate ::ast/literal
  [{::ast/keys [value]}]
  value)

(defmethod evaluate ::ast/grouping
  [{::ast/keys [expression]}]
  (evaluate expression))

(defmethod evaluate ::ast/unary
  [{::ast/keys [operator right]}]
  (let [r (evaluate right)]
    (case (::token/type operator)
      ::token/bang (not r)
      ::token/minus (numeric-operator operator - r))))

(defmethod evaluate ::ast/binary
  [{::ast/keys [left operator right]}]
  (let [l (evaluate left)
        r (evaluate right)]
    (case (::token/type operator)
      ::token/minus (numeric-operator operator - l r)
      ::token/plus (add operator l r)
      ::token/slash (divide operator l r)
      ::token/star (numeric-operator operator * l r)
      ::token/bang-eq (not= l r)
      ::token/eq-eq (= l r)
      ::token/gt (numeric-operator operator > l r)
      ::token/gte (numeric-operator operator >= l r)
      ::token/lt (numeric-operator operator < l r)
      ::token/lte (numeric-operator operator <= l r)
      ::token/comma r)))

(defmethod evaluate ::ast/ternary
  [{::ast/keys [test then else]}]
  (if (evaluate test)
    (evaluate then)
    (evaluate else)))

(defn interpret
  "Interprets `ast` into a map of ::result and ::error"
  [ast]
  (try
    {::result (evaluate ast) ::error nil}
    (catch clojure.lang.ExceptionInfo e
      {::result nil ::error (ex-data e)})))
