(ns cljox.parser
  (:refer-clojure :exclude [peek])
  (:require [cljox.error :as error]
            [cljox.ast :as ast]
            [cljox.token :as token]))

(defn- with-location
  "Appends to `message` a string indicating the token that triggered `error`"
  [message {::keys [token] :as error}]
  (if (= ::token/eof (::token/type token))
    (str message " at end")
    (format "%s at '%s'" message (::token/lexeme token))))

(defmacro error-formatter
  [type message]
  `(defmethod error/fmt ~type
     [~'error]
     (with-location ~message ~'error)))

(error-formatter ::missing-closing-paren "expected ')'")

(error-formatter ::missing-expression "expected expression")

(error-formatter ::missing-binary-operand "expected left operand to binary operator")

(defn- parser
  "Creates a parser from `tokens`"
  [tokens]
  #::{:tokens tokens
      :expression nil
      :errors []
      :curr 0})

(defn- peek
  "Returns `parser`'s current token"
  [{::keys [curr tokens] :as parser}]
  (get tokens curr))

(defn- matches?
  "Returns true if `parser`'s current token is one of `token-types`"
  [parser & token-types]
  (let [curr-type (::token/type (peek parser))]
    (boolean (some #{curr-type} token-types))))

(defn- at-end?
  "Returns true if `parser` has consumed its tokens, false otherwise"
  [parser]
  (matches? parser ::token/eof))

(defn- advance
  "Advances `parser` by one token"
  [parser]
  (if (at-end? parser)
    parser
    (update parser ::curr inc)))

(defn- add-expression
  "Adds `expression` to `parser`"
  [parser expression]
  (assoc parser ::expression expression))

(defn- add-literal
  "Adds a literal expression with value `value`, or the current literal
  in `parser`, to `parser` and advances it"
  ([parser]
   (->> parser
        peek
        ::token/literal
        (add-literal parser)))
  ([parser value]
   (->> value
        ast/literal
        (add-expression parser)
        advance)))

(defn- add-error
  "Adds an error of type `type` to `parser`"
  [{::keys [tokens curr] :as parser} type]
  (let [{::token/keys [start end]} (peek parser)]
    (update parser ::errors conj (error/error type start end
                                              ::token (get tokens curr)))))

(defn- add-error-throw
  "Adds an error of type `type` to `parser` and throws it inside an [[ExceptionInfo]]"
  [parser type]
  (throw (ex-info "parsing error" (add-error parser type))))

(defn- synchronize
  "Synchronizes `parser` when it is in a panic state after a parse error"
  [parser]
  (cond
    (at-end? parser) parser
    (matches? parser ::token/semicolon) (advance parser)
    (matches? parser
              ::token/class ::token/fun ::token/var ::token/for
              ::token/if ::token/while ::token/print ::token/print) parser
    :else (recur (advance parser))))

(defn- expect
  "Adds an error to `parser` and throws it inside an [[ExceptionInfo]]
  if `expected` is not `parser`'s current token"
  [parser expected error]
  (if (matches? parser expected)
    (advance parser)
    (add-error-throw parser error)))

(declare expression)

(defn- primary
  "Parses the current primary expression into `parser`"
  [parser]
  (cond
    (matches? parser ::token/false) (add-literal parser false)
    (matches? parser ::token/true) (add-literal parser true)
    (matches? parser ::token/nil) (add-literal parser nil)
    (matches? parser ::token/number) (add-literal parser)
    (matches? parser ::token/string) (add-literal parser)
    (matches? parser ::token/left-paren) (let [middle (expression (advance parser))]
                                           (add-expression
                                            (expect middle ::token/right-paren ::missing-closing-paren)
                                            (ast/grouping (::expression middle))))
    :else (add-error-throw parser ::missing-expression)))

(defn- unary
  "Parses the current unary expression into `parser`"
  [parser]
  (if (matches? parser ::token/bang ::token/minus)
    (let [operator (peek parser)
          right (unary (advance parser))]
      (add-expression right (ast/unary operator (::expression right))))
    (primary parser)))

(defn- binary
  "Parses the current binary expression into `parser`

  `operand` is a function parsing the operator's operands
  `operators` is a sequence of token types representing the expression's operator"
  [parser operand & operators]
  (loop [left (if (apply matches? parser operators)
                (add-error parser ::missing-binary-operand)
                (operand parser))]
    (if (apply matches? left operators)
      (let [operator (peek left)
            right (operand (advance left))]
        (recur (add-expression right (ast/binary (::expression left)
                                                 operator
                                                 (::expression right)))))
      left)))

(defn- factor
  "Parses the current factor expression into `parser`"
  [parser]
  (binary parser unary ::token/slash ::token/star))

(defn- term
  "Parses the current term expression into `parser`"
  [parser]
  (loop [left (if (matches? parser ::token/plus)
                (add-error parser ::missing-binary-operand)
                (factor parser))]
    (if (matches? left ::token/minus ::token/plus)
      (let [operator (peek left)
            right (factor (advance left))]
        (recur (add-expression right (ast/binary (::expression left)
                                                 operator
                                                 (::expression right)))))
      left)))

(defn- comparison
  "Parses the current comparison expression into `parser`"
  [parser]
  (binary parser term ::token/gt ::token/gte ::token/lt ::token/lte))

(defn- equality
  "Parses the current equality expression into `parser`"
  [parser]
  (binary parser comparison ::token/bang-eq ::token/eq-eq))

(defn- ternary
  "Parses the current ternary expression into `parser`"
  [parser]
  (let [test (equality parser)]
    (if (matches? test ::token/question)
      (let [then (expression (advance test))]
        (if (matches? then ::token/colon)
          (let [else (ternary (advance then))]
            (add-expression else (apply ast/ternary (map ::expression [test then else]))))
          (add-error-throw then ::missing-ternary-colon)))
      test)))

(defn- comma
  "Parses the current comma expression into `parser`"
  [parser]
  (binary parser ternary ::token/comma))

(defn- expression
  "Parses the current top-level expression into `parser`"
  [parser]
  (comma parser))

(defn parse
  "Parses `tokens` into a map of ::expression and ::errors"
  [tokens]
  (-> tokens
      parser
      expression
      (try (catch clojure.lang.ExceptionInfo e (ex-data e)))
      (select-keys [::expression ::errors])))
