(ns cljox.core
  (:gen-class)
  (:require [cljox.error :as error]
            [cljox.scanner :as scanner]))

(defn- print-errors
  [errors]
  (doseq [error errors] (println (error/pretty-str error))))

(defn run
  "Executes `input` as a Lox program"
  [input]
  (let [{::scanner/keys [tokens errors]} (scanner/scan input)]
    (if (seq errors)
      (print-errors errors)
      (doseq [token tokens] (println token)))))

(defn run-file
  "Executes `file` as a Lox program"
  [file]
  (run (slurp file)))

(defn run-prompt
  "Runs an interactive prompt to execute Lox lines until EOF"
  []
  (println "> ")
  (flush)
  (when-let [line (read-line)]
    (run line)
    (recur)))

(defn -main
  "Executes the Lox interpreter"
  ([] (run-prompt))
  ([file] (run-file file))
  ([_ _ & _] (println "Usage: cljox [script]")))
