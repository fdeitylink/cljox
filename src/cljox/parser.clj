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

(defmacro ^:private error-formatter
  [type message]
  `(defmethod error/fmt ~type
     [~'error]
     (with-location ~message ~'error)))

(error-formatter ::missing-closing-paren "expected ')'")

(error-formatter ::missing-expression "expected expression")

(error-formatter ::missing-binary-operand "expected left operand to binary operator")

(error-formatter ::missing-ternary-colon "expected colon after then-clause of ternary expression")

(error-formatter ::invalid-assignment-target "invalid assignment target")

(error-formatter ::missing-semicolon "expected ';'")

(error-formatter ::missing-closing-brace "expected '}")

(error-formatter ::missing-if-left-paren "expected '(' after 'if'")

(error-formatter ::missing-if-right-paren "expected ')' after if condition")

(error-formatter ::missing-while-left-paren "expected '(' after 'while'")

(error-formatter ::missing-while-right-paren "expected ')' after while condition")

(error-formatter ::missing-for-left-paren "expected '(' after 'for'")

(error-formatter ::missing-for-test-semicolon "expected ';' after for condition")

(error-formatter ::missing-for-right-paren "expected ')' after for clauses")

(error-formatter ::break-outside-loop "unexpected 'break' outside loop body")

(error-formatter ::missing-var-name "expected variable name")

(defn- parser
  "Creates a parser from `tokens`"
  [tokens]
  #::{:tokens tokens
      :expression nil
      :statements []
      :errors []
      :curr 0
      :loop-depth 0})

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
  if `expected` is not `parser`'s current token, otherwise advances it"
  [parser expected error]
  (if (matches? parser expected)
    (advance parser)
    (add-error-throw parser error)))

(defn- enter-loop-body
  "Increments the loop depth of `parser`"
  [parser]
  (update parser ::loop-depth inc))

(defn- exit-loop-body
  "Decrements the loop depth of `parser`"
  [parser]
  (update parser ::loop-depth dec))

(defn- inside-loop-body?
  "Returns true if the parser is inside a loop body, false otherwise"
  [parser]
  (pos? (::loop-depth parser)))

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
    (matches? parser ::token/identifier) (add-expression (advance parser) (ast/var (peek parser)))
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

(defn- binary*
  "Parses the current binary expression into `parser`

  `ast` is a function that creates an AST node of the left operand, operator, and right operand
  `operand` is a function parsing the operator's operands
  `operators` is a sequence of token types representing the expression's operator"
  [parser ast operand & operators]
  (loop [left (if (apply matches? parser operators)
                (add-error parser ::missing-binary-operand)
                (operand parser))]
    (if (apply matches? left operators)
      (let [operator (peek left)
            right (operand (advance left))]
        (recur (add-expression right (ast (::expression left)
                                          operator
                                          (::expression right)))))
      left)))

(defn- binary
  "Parses the current binary expression into `parser`

  `operand` is a function parsing the operator's operands
  `operators` is a sequence of token types representing the expression's operator"
  [parser operand & operators]
  (apply binary* parser ast/binary operand operators))

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

(defn- logical-and
  "Parses the current logical and expression into `parser`"
  [parser]
  (binary* parser ast/logical equality ::token/and))

(defn- logical-or
  "Parses the current logical or expression into `parser`"
  [parser]
  (binary* parser ast/logical logical-and ::token/or))

(defn- ternary
  "Parses the current ternary expression into `parser`"
  [parser]
  (let [test (logical-or parser)]
    (if (matches? test ::token/question)
      (let [then (expression (advance test))
            else (ternary (expect then ::token/colon ::missing-ternary-colon))]
          (add-expression else (apply ast/ternary (map ::expression [test then else]))))
      test)))

(defn- assignment
  "Parses the current assignment expression into `parser`"
  [parser]
  (let [expr (ternary parser)]
    (if (matches? expr ::token/eq)
      (if (= ::ast/var (-> expr ::expression ::ast/type))
       (let [value (assignment (advance expr))]
         (add-expression value (ast/assignment (-> expr ::expression ::ast/name)
                                               (::expression value))))
       (assignment (advance (add-error expr ::invalid-assignment-target))))
      expr)))

(defn- comma
  "Parses the current comma expression into `parser`"
  [parser]
  (binary parser assignment ::token/comma))

(defn- expression
  "Parses the current top-level expression into `parser`"
  [parser]
  (comma parser))

(declare statement)
(declare var-declaration)
(declare declaration)

(defn- expression-statement
  "Parses the current expression statement into `parser`"
  [parser]
  (let [expr (expression parser)
        semi (expect expr ::token/semicolon ::missing-semicolon)]
    (add-expression semi (ast/expression-statement (::expression expr)))))

(defn- print-statement
  "Parses the current print statement into `parser`"
  [parser]
  (let [expr (expression parser)
        semi (expect expr ::token/semicolon ::missing-semicolon)]
    (add-expression semi (ast/print-statement (::expression expr)))))

(defn- block
  "Parses the current block statement into `parser`"
  [parser]
  (loop [parser parser
         statements []]
    (if (or (matches? parser ::token/right-brace)
            (at-end? parser))
      (add-expression
       (expect parser ::token/right-brace ::missing-closing-brace)
       (ast/block statements))
      (let [stmt (declaration parser)]
        (recur stmt (conj statements (::expression stmt)))))))

(defn- if-statement
  "Parses the current if statement into `parser`"
  [parser]
  (let [test (expression (expect parser ::token/left-paren ::missing-if-left-paren))
        then (statement (expect test ::token/right-paren ::missing-if-right-paren))
        else (when (matches? then ::token/else) (statement (advance then)))]
    (add-expression (or else then) (apply ast/if-statement (map ::expression [test then else])))))

(defn- while-statement
  "Parses the current while statement into `parser`"
  [parser]
  (let [test (expression (expect parser ::token/left-paren ::missing-while-left-paren))
        body (-> test
                 (expect ::token/right-paren ::missing-while-right-paren)
                 enter-loop-body
                 statement
                 exit-loop-body)]
    (add-expression body (ast/while-statement (::expression test) (::expression body)))))

(defn- for-statement
  "Parses the current for statement into `parser`"
  [parser]
  (let [left-paren (expect parser ::token/left-paren ::missing-for-left-paren)
        initializer (cond
                      (matches? left-paren ::token/semicolon) (advance left-paren)
                      (matches? left-paren ::token/var) (var-declaration (advance left-paren))
                      :else (expression-statement left-paren))
        test (if (matches? initializer ::token/semicolon)
               (assoc initializer ::expression nil)
               (expression initializer))
        semi (expect test ::token/semicolon ::missing-for-test-semicolon)
        increment (if (matches? semi ::token/right-paren)
                    (assoc semi ::expression nil)
                    (expression semi))
        body (-> increment
                 (expect ::token/right-paren ::missing-for-right-paren)
                 enter-loop-body
                 statement
                 exit-loop-body)
        [init t inc stmt] (map ::expression [initializer test increment body])
        stmt (if inc (ast/block [stmt (ast/expression-statement inc)]) stmt)
        stmt (ast/while-statement (or t (ast/literal true)) stmt)
        stmt (if init (ast/block [init stmt]) stmt)]
    (add-expression body stmt)))

(defn- break-statement
  "Parses the current break statement into `parser`

  The current token should be ::token/break"
  [parser]
  (let [break (if (inside-loop-body? parser)
                (add-expression (advance parser) (ast/break-statement))
                (advance (add-error parser ::break-outside-loop)))]
     (expect break ::token/semicolon ::missing-semicolon)))

(defn- statement
  "Parses the current top-level statement into `parser`"
  [parser]
  (cond
    (matches? parser ::token/left-brace) (block (advance parser))
    (matches? parser ::token/print) (print-statement (advance parser))
    (matches? parser ::token/if) (if-statement (advance parser))
    (matches? parser ::token/while) (while-statement (advance parser))
    (matches? parser ::token/for) (for-statement (advance parser))
    (matches? parser ::token/break) (break-statement parser)
    :else (expression-statement parser)))

(defn- var-declaration
  "Parses the current variable declaration into `parser`"
  [parser]
  (let [name (expect parser ::token/identifier ::missing-var-name)
        initializer (when (matches? name ::token/eq) (expression (advance name)))
        semi (expect (or initializer name) ::token/semicolon ::missing-semicolon)]
    (add-expression semi (ast/var-statement (peek parser) (::expression initializer)))))

(defn- declaration
  "Parses the current declaration into `parser`"
  [parser]
  (try
    (cond
      (matches? parser ::token/var) (var-declaration (advance parser))
      :else (statement parser))
    (catch clojure.lang.ExceptionInfo e
      (synchronize (ex-data e)))))

(defn parse
  "Parses `tokens` into a map of ::statements and ::errors"
  [tokens]
  (loop [parser (parser tokens)]
    (if (at-end? parser)
      (select-keys parser [::statements ::errors])
      (let [stmt (declaration parser)]
        (-> stmt
            (update ::statements conj (::expression stmt))
            (assoc ::expression nil)
            recur)))))
