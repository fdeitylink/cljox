(ns cljox.ast
  (:require [cljox.token :as token]
            clojure.string))

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

(defmulti pretty-str
  "Converts an ast into a parenthesized Polish form string"
  ::type)

(defn parenthesize
  "Parenthesizes `name` and `expressions` as a string"
  [name & expressions]
  (str "(" name " " (clojure.string/join " " (map pretty-str expressions)) ")"))

(defmethod pretty-str ::literal
  [{::keys [expression]}]
  (pr-str expression))

(defmethod pretty-str ::grouping
  [{::keys [expression]}]
  (parenthesize "group" expression))

(defmethod pretty-str ::unary
  [{::keys [operator right]}]
  (parenthesize (::token/lexeme operator) right))

(defmethod pretty-str ::binary
  [{::keys [left operator right]}]
  (parenthesize (::token/lexeme operator) left right))

(defmethod pretty-str ::ternary
  [{::keys [test then else]}]
  (parenthesize "ternary" test then else))
