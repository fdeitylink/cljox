(ns cljox.error
  (:require [cljox.location :as location]))

(defn error
  "Creates a new error with the given `type` and `start` and `end`
  locations and any extra properties in `keyvals`."
  [type start end & keyvals]
  (apply array-map ::type type ::start start ::end end keyvals))

(defmulti fmt
  "Returns an error message according to the error's type"
  ::type)

(defn pretty-str
  "Formats `error` as a string with location and message"
  [{::keys [start end] :as error}]
  (str (location/pretty-str start end) ": " (fmt error)))
