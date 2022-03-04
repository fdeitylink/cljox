(ns cljox.ast)

(defn literal
  "Creates a literal expression of `value`"
  [value]
  #::{:type ::literal
      :value value})

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

(defn ternary
  "Creates a ternary expression of `test` `then` `else`"
  [test then else]
  #::{:type ::ternary
      :test test
      :then then
      :else else})
