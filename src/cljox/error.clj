(ns cljox.error
  (:require [cljox.location :as location]
            [cljox.token :as token]))

(defmulti fmt
  "Returns an error message according to the error's type"
  ::type)

(defn pretty-str
  "Formats `error` as a string with location and message"
  [{::keys [start end] :as error}]
  (str (location/pretty-str start end) ": " (fmt error)))

(defn error
  "Creates a new error with the given `type` and `start` and `end`
  locations and any extra properties in `keyvals`."
  [type start end & keyvals]
  (apply array-map ::type type ::start start ::end end keyvals))

(defn runtime-error
  "Creates an error of type `type` located at `token` with any extra
  properties in `keyvals` and throws it inside an [[ExceptionInfo]]"
  [type token & keyvals]
  (let [error (apply error type (::token/start token) (::token/end token) keyvals)]
    (throw (ex-info (fmt error) error))))
