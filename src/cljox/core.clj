(ns cljox.core
  (:gen-class)
  (:require [cljox.ast :as ast]
            [cljox.error :as error]
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
  [input]
  (let [{::scanner/keys [tokens errors]} (scanner/scan input)]
    (if (seq errors)
      (print-errors errors)
      (let [{::parser/keys [expression errors]} (parser/parse tokens)]
        (if (seq errors)
          (print-errors errors)
          (let [{::interpreter/keys [result error]} (interpreter/interpret expression)]
            (if error
              (print-error error)
              (prn result))))))))

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
