(ns cljox.ast)

(defn literal
  "Creates a literal expression of `value`"
  [value]
  #::{:type ::literal
      :value value})

(defn var
  "Creates a variable expression of `name`"
  [name]
  #::{:type ::var
      :name name})

(defn grouping
  "Creates a grouping expression of `expression`"
  [expression]
  #::{:type ::grouping
      :expression expression})

(defn unary
  "Creates a unary expression of `operator` `right`"
  [operator right]
  #::{:type ::unary
      :operator operator
      :right right})

(defn binary
  "Creates a binary expression of `left` `operator` `right`"
  [left operator right]
  #::{:type ::binary
      :left left
      :operator operator
      :right right})

(defn logical
  "Creates a logical expression of `left` `operator` `right`"
  [left operator right]
  #::{:type ::logical
      :left left
      :operator operator
      :right right})

(defn ternary
  "Creates a ternary expression of `test` `then` `else`"
  [test then else]
  #::{:type ::ternary
      :test test
      :then then
      :else else})

(defn assignment
  "Creates an assignment expression of `name` `value`"
  [name value]
  #::{:type ::assignment
      :name name
      :value value})

(defn expression-statement
  "Creates an expresion statement of `expression`"
  [expression]
  #::{:type ::expression-statement
      :expression expression})

(defn print-statement
  "Creates a print statement of `expression`"
  [expression]
  #::{:type ::print-statement
      :expression expression})

(defn block
  "Creates a block statement of `statements`"
  [statements]
  #::{:type ::block
      :statements statements})

(defn if-statement
  "Creates an if statement of `test` `then` `else`"
  [test then else]
  #::{:type ::if-statement
      :test test
      :then then
      :else else})

(defn while-statement
  "Creates a while statement of `test` `body`"
  [test body]
  #::{:type ::while-statement
      :test test
      :body body})

(defn break-statement
  "Creates a break statement"
  []
  #::{:type ::break-statement})

(defn var-statement
  "Creates a declaration statement of `name` `initializer`"
  [name initializer]
  #::{:type ::var-statement
      :name name
      :initializer initializer})
