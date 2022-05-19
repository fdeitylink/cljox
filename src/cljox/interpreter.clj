(ns cljox.interpreter
  (:require [cljox.ast :as ast]
            [cljox.environment :as environment]
            [cljox.error :as error]
            [cljox.token :as token]
            [clojure.string :as s]))

(defn- stringify
  "Formats `value` as a string"
  [value]
  (cond
    (nil? value) "nil"
    (number? value) (let [s (str value)]
                      (if (s/ends-with? s ".0")
                        (subs s 0 (- (count s) 2))
                        s))
    (string? value) value
    :else (str value)))

(defmethod error/fmt ::nonnumeric-unary-operand
  [{::keys [operator right] :as error}]
  (format "operand '%s' to '%s' must be numeric" (stringify right) (::token/lexeme operator)))

(defmethod error/fmt ::nonnumeric-binary-operands
  [{::keys [operator left right] :as error}]
  (format "operands '%s', '%s' to '%s' must be numeric" (stringify left) (stringify right) (::token/lexeme operator)))

(defmethod error/fmt ::invalid-plus-operands
  [{::keys [left right] :as error}]
  (format "operands '%s', '%s' to + must both be numeric or one must be a string" (stringify left) (stringify right)))

(defmethod error/fmt ::division-by-zero
  [{::keys [left] :as error}]
  (format "division of '%s' by 0" (pr-str left)))

(defn state
  "Creates a new interpreter state"
  []
  #::{:result nil
      :environment (atom (environment/environment))
      :error nil})

(defn clear-error
  [state]
  (assoc state ::error nil))

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
    (or (string? l) (string? r)) (str (stringify l) (stringify r))
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

(defmulti ^:private evaluate
  (fn [state ast] (::ast/type ast)))

(defmethod evaluate ::ast/literal
  [state {::ast/keys [value]}]
  (assoc state ::result value))

(defmethod evaluate ::ast/var
  [{::keys [environment] :as state} {::ast/keys [name]}]
  (assoc state ::result (environment/get-var @environment name)))

(defmethod evaluate ::ast/grouping
  [state {::ast/keys [expression]}]
  (evaluate state expression))

(defmethod evaluate ::ast/unary
  [state {::ast/keys [operator right]}]
  (let [state (evaluate state right)
        r (::result state)]
    (assoc state ::result
           (case (::token/type operator)
             ::token/bang (not r)
             ::token/minus (numeric-operator operator - r)))))

(defmethod evaluate ::ast/binary
  [state {::ast/keys [left operator right]}]
  (let [state (evaluate state left)
        l (::result state)
        state (evaluate state right)
        r (::result state)]
    (assoc state ::result
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
             ::token/comma r))))

(defmethod evaluate ::ast/logical
  [state {::ast/keys [left operator right]}]
  (let [state (evaluate state left)
        l (::result state)
        tok (::token/type operator)]
    (cond
      (and l (= tok ::token/or)) state
      (and (not l) (= tok ::token/and)) state
      :else (evaluate state right))))

(defmethod evaluate ::ast/ternary
  [state {::ast/keys [test then else]}]
  (let [state (evaluate state test)]
    (evaluate state (if (::result state) then else))))

(defmethod evaluate ::ast/assignment
  [state {::ast/keys [name value]}]
  (let [state (evaluate state value)
        env (::environment state)
        val (::result state)]
    (swap! env environment/assign-var name val)
    (assoc state ::result val)))

(defmethod evaluate ::ast/expression-statement
  [state {::ast/keys [expression]}]
  (-> state
      (evaluate expression)
      (assoc ::result nil)))

(defmethod evaluate ::ast/print-statement
  [state {::ast/keys [expression]}]
  (let [state (evaluate state expression)]
    (-> state ::result stringify println)
    (assoc state ::result nil)))

(defmethod evaluate ::ast/block
  [{::keys [environment] :as state} {::ast/keys [statements]}]
  (swap! environment environment/push-scope)
  (try
    (reduce evaluate state statements)
    (finally
      (swap! environment environment/pop-scope)
      state)))

(defmethod evaluate ::ast/if-statement
  [state {::ast/keys [test then else]}]
  (let [state (evaluate state test)]
    (cond
      (::result state) (evaluate state then)
      else (evaluate state else)
      :else (assoc state ::result nil))))

(defmethod evaluate ::ast/while-statement
  [state {::ast/keys [test body] :as while}]
  (let [state (evaluate state test)]
    (if (::result state)
      (let [[state break] (try
                            [(evaluate (assoc state ::result nil) body) false]
                            (catch clojure.lang.ExceptionInfo e
                              (if (= ::break (::type (ex-data e)))
                                [(::state (ex-data e)) true]
                                (throw e))))]
        (if break state (recur state while)))
      state)))

(defmethod evaluate ::ast/break-statement
  [state _]
  (throw (ex-info "break" {::type ::break ::state state})))

(defmethod evaluate ::ast/var-statement
  [state {::ast/keys [name initializer]}]
  (let [state (if initializer (evaluate state initializer) state)
        env (::environment state)]
    (if initializer
      (swap! env environment/define-var name (::result state))
      (swap! env environment/declare-var name))
    (assoc state ::result nil)))

(defn interpret
  "Interprets `statements` and returns the new interpreter state"
  [state statements]
  (reduce
   (fn [state statement]
     (try
       (evaluate state statement)
       (catch clojure.lang.ExceptionInfo e
         (if (contains? (ex-data e) ::error/type)
           (reduced (assoc state ::error (ex-data e) ::result nil))
           (throw e)))))
   state
   statements))
