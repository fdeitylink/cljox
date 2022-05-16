(ns cljox.scanner
  (:refer-clojure :exclude [peek])
  (:require [cljox.error :as error]
            [cljox.location :as location]
            [cljox.token :as token]))

(defmethod error/fmt ::unterminated-comment
  [{::keys [input start]}]
  (str "unterminated comment: " (subs input start)))

(defmethod error/fmt ::unterminated-string
  [{::keys [input start]}]
  (str "unterminated string: " (subs input start)))

(defmethod error/fmt ::invalid-number
  [{::keys [input start curr]}]
  (str "invalid number: " (subs input start curr)))

(defmethod error/fmt ::unexpected-character
  [{::keys [input start]}]
  (str "unexpected character: " (get input start)))

(defn- scanner
  "Creates a scanner from `input`"
  [input]
  #::{:input input
      :tokens []
      :errors []
      :start 0
      :curr 0
      :start-loc (location/location)
      :curr-loc (location/location)})

(defn- at-end?
  "Returns true if `scanner` has consumed `input`, false otherwise"
  [{::keys [curr input] :as scanner}]
  (>= curr (count input)))

(defn- advance-start
  "Advances `scanner`'s start location to the current end"
  [{::keys [curr curr-loc] :as scanner}]
  (assoc scanner
         ::start curr
         ::start-loc curr-loc))

(defn- lexeme
  "Returns `scanner`'s current lexeme"
  [{::keys [input start curr] :as scanner}]
  (subs input start curr))

(defn- peek
  "Returns the character in `input` at `curr` plus `offset`, or 0"
  ([scanner]
   (peek scanner 0))
  ([{::keys [input curr] :as scanner} offset]
   (get input (+ curr offset))))

(defn- advance
  "Advances `scanner` by `offset` characters, or 1"
  ([scanner]
   (advance scanner 1))
  ([scanner offset]
   (->> scanner
        (iterate #(-> %
                      (update ::curr inc)
                      (update ::curr-loc (if (= \newline (peek %))
                                           location/advance-line
                                           location/advance-col))))
        (drop offset)
        first)))

(defn- peek-and-advance
  "Returns a vector with the results of peeking and advancing `scanner`"
  [scanner]
  [(peek scanner) (advance scanner)])

(defn- match
  "Advances `scanner` if its current character is `expect`, nil otherwise"
  [scanner expect]
  (when (= expect (peek scanner))
    (advance scanner)))

(defn- add-token
  "Adds a token of type `type` with literal `literal`, or nil, to `scanner`"
  ([scanner type]
   (add-token scanner type nil))
  ([{::keys [start-loc curr-loc] :as scanner} type literal]
   (update scanner ::tokens conj (token/token type (lexeme scanner) literal start-loc curr-loc))))

(defn- match-add-token
  "Matches `scanner` on `expect` and adds a token of type `matched` if possible,
  otherwise adds a token of type `default`"
  [scanner expect matched default]
  (if-let [scanner (match scanner expect)]
    (add-token scanner matched)
    (add-token scanner default)))

(defn- add-error
  "Adds an error of type `type` to `scanner`"
  [{::keys [input start curr start-loc curr-loc] :as scanner} type]
  (update scanner ::errors conj (error/error type start-loc curr-loc
                                             ::input input ::start start ::curr curr)))

(defn- alpha?
  "Returns true if `c` is '_' or an alphabetical character, false otherwise"
  [c]
  (and (char? c)
       (or
        (= \_ c)
        (<= (int \A) (int c) (int \Z))
        (<= (int \a) (int c) (int \z)))))

(defn- digit?
  "Returns true if `c` is a digit character, false otherwise"
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

(def ^:private alphanumeric?
  "Returns true if `c` is an alphanumeric character, false otherwise"
  (some-fn alpha? digit?))

(defn- advance-while
  "Advances `scanner` while calling `pred` on it returns logical `true`"
  [scanner pred]
  (->> scanner
       (iterate advance)
       (drop-while pred)
       first))

(defn- advance-until
  "Advances `scanner` until calling `pred` on it returns logical `true`"
  [scanner pred]
  (advance-while scanner (complement pred)))

(defn- advance-until-delimeter
  "Advances `scanner` until `delimeter` is reached or the input is exhausted"
  [scanner delimeter]
  (advance-until scanner (some-fn at-end? (comp #{delimeter} peek))))

(defn- start-of-block-comment?
  "Returns true if `scanner`'s current input starts with '/*', false otherwise"
  [scanner]
  (and (= \/ (peek scanner)) (= \* (peek scanner 1))))

(defn- end-of-block-comment?
  "Returns true if `scanner`'s current input starts with '*/', false otherwise"
  [scanner]
  (and (= \* (peek scanner)) (= \/ (peek scanner 1))))

(defn- discard-block-comment
  "Discards the (possibly nested) block comment at the top of `scanner`'s input"
  [scanner]
  (let [scanner (advance-until scanner (some-fn at-end?
                                                start-of-block-comment?
                                                end-of-block-comment?))]
    (cond
      ;; expected to find comment delimeter, raise error
      (at-end? scanner) (add-error scanner ::unterminated-comment)
      ;; found comment end delimeter, advance past it
      (end-of-block-comment? scanner) (advance scanner 2)
      ;; found comment start delimeter
      ;; advance past it
      ;; recur to skip that nested comment
      ;; recur again to find the end of this comment or another nested comment
      (start-of-block-comment? scanner) (-> scanner
                                            (advance 2)
                                            discard-block-comment
                                            recur))))

(defn- add-string
  "Scans the string at the start of `scanner`'s current input"
  [scanner]
  (let [scanner (advance-until-delimeter scanner \")]
    (if (at-end? scanner)
      (add-error scanner ::unterminated-string)
      (let [scanner (advance scanner)]
        (add-token scanner ::token/string
                   (let [lex (lexeme scanner)]
                     (subs lex 1 (dec (count lex)))))))))

(defn- add-number
  "Scans the number at the start of `scanner`'s current input"
  [scanner]
  (let [scanner (advance-while scanner (comp digit? peek))]
    (if (and (= \. (peek scanner)) (digit? (peek scanner 1)))
      (-> scanner
          advance
          (advance-while (comp digit? peek)))
      scanner)
    (try
      (add-token scanner ::token/number (Double/parseDouble (lexeme scanner)))
      (catch NumberFormatException _ (add-error scanner ::invalid-number)))))

(def ^:private reserved-words
  "Set of reserved words in Lox"
  #{"and"
    "class"
    "else"
    "false"
    "fun"
    "for"
    "if"
    "nil"
    "or"
    "print"
    "return"
    "super"
    "this"
    "true"
    "var"
    "while"})

(defn- add-identifier
  "Scans the reserved word or identifier at the start of `scanner`'s current input"
  [scanner]
  (let [scanner (advance-while scanner (comp alphanumeric? peek))
        identifier (lexeme scanner)]
    (if (reserved-words identifier)
      (add-token scanner (keyword "cljox.token" identifier))
      (add-token scanner ::token/identifier identifier))))

(defn- scan-token
  "Scans the token at the start of `scanner`'s current input"
  [scanner]
  (let [[c scanner] (peek-and-advance scanner)]
    (case c
      \( (add-token scanner ::token/left-paren)
      \) (add-token scanner ::token/right-paren)
      \{ (add-token scanner ::token/left-brace)
      \} (add-token scanner ::token/right-brace)
      \, (add-token scanner ::token/comma)
      \. (add-token scanner ::token/dot)
      \- (add-token scanner ::token/minus)
      \+ (add-token scanner ::token/plus)
      \; (add-token scanner ::token/semicolon)
      \* (add-token scanner ::token/star)
      \? (add-token scanner ::token/question)
      \: (add-token scanner ::token/colon)
      \! (match-add-token scanner \= ::token/bang-eq ::token/bang)
      \= (match-add-token scanner \= ::token/eq-eq ::token/eq)
      \< (match-add-token scanner \= ::token/lte ::token/lt)
      \> (match-add-token scanner \= ::token/gte ::token/gt)
      \/ (if-let [scanner (match scanner \/)]
           (advance-until-delimeter scanner \newline)
           (if-let [scanner (match scanner \*)]
             (discard-block-comment scanner)
             (add-token scanner ::token/slash)))
      \" (add-string scanner)
      nil scanner
      (cond
        (Character/isWhitespace c) scanner
        (digit? c) (add-number scanner)
        (alpha? c) (add-identifier scanner)
        :else (add-error scanner ::unexpected-character)))))

(defn scan
  "Scans `input` into a map of ::tokens and ::errors"
  [input]
  (loop [scanner (scanner input)]
    (if (at-end? scanner)
      (-> scanner
          advance-start
          (add-token ::token/eof)
          (select-keys [::tokens ::errors]))
      (-> scanner advance-start scan-token recur))))
