(ns cljox.location)

(defn location
  "Creates a location with `line` and `col`, or 1 and 0"
  ([] (location 1 0))
  ([line col]
   #::{:line line
       :col col}))

(defn advance-col
  "Advances the column of `location`"
  [location]
  (update location ::col inc))

(defn advance-line
  "Advances the line and resets the column of `location`"
  [location]
  (-> location
      (update ::line inc)
      (assoc ::col 0)))

(defn pretty-str
  "Formats `location` as a string"
  ([{::keys [line col] :as location}]
   (format "@ %d:%d" line col))
  ([{start-line ::line start-col ::col :as start} {end-line ::line end-col ::col :as end}]
   (if (= start-line end-line)
     (format "@%d:[%d, %d)" start-line start-col end-col)
     (format "@[%d:%d, %d:%d)" start-line start-col end-line end-col))))
