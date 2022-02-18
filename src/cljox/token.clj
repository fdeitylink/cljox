(ns cljox.token)

(def types
  "Set of valid token types"
  #{::left-paren ::right-paren
    ::left-brace ::right-brace
    ::comma ::dot
    ::minus ::plus
    ::semicolon
    ::slash ::star
    ::question ::colon

    ::bang ::bang-eq
    ::eq ::eq-eq
    ::gt ::gte
    ::lt ::lte

    ::identifier
    ::string
    ::number

    ::and
    ::class
    ::else
    ::false
    ::fun
    ::for
    ::if
    ::nil
    ::or
    ::print
    ::return
    ::super
    ::this
    ::true
    ::var
    ::while

    ::eof})

(defn token
  "Creates a token with the given properties"
  [type lexeme literal start end]
  #::{:type type
      :lexeme lexeme
      :literal literal
      :start start
      :end end})
