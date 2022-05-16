(ns cljox.core
  (:gen-class)
  (:require [cljox.error :as error]
            [cljox.interpreter :as interpreter]
            [cljox.parser :as parser]
            [cljox.scanner :as scanner]))

(defn- print-error
  "Formats and prints `error` to stderr"
  [error]
  (binding [*out* *err*]
    (println (error/pretty-str error))))

(defn- print-errors
  "Prints each error in `errors`"
  [errors]
  (dorun (map print-error errors)))

(defn run
  "Executes `input` as a Lox program"
  [state input]
  (let [{::scanner/keys [tokens errors]} (scanner/scan input)]
    (if (seq errors)
      (print-errors errors)
      (let [{::parser/keys [statements errors]} (parser/parse tokens)]
        (if (seq errors)
          (print-errors errors)
          (let [{::interpreter/keys [error] :as state} (interpreter/interpret state statements)]
            (if error
              (do
                (print-error error)
                (interpreter/clear-error state))
              state)))))))

(defn run-file
  "Executes `file` as a Lox program"
  [file]
  (run (interpreter/state) (slurp file)))

(defn run-prompt
  "Runs an interactive prompt to execute Lox lines until EOF"
  []
  (loop [state (interpreter/state)]
    (print "> ")
    (flush)
    (when-let [line (read-line)]
      (recur (run state line)))))

(defn -main
  "Executes the Lox interpreter"
  ([] (run-prompt))
  ([file] (run-file file))
  ([_ _ & _] (println "Usage: cljox [script]")))
