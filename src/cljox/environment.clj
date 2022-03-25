(ns cljox.environment
  (:require [cljox.error :as error]
            [cljox.token :as token]))

(defmethod error/fmt ::undefined-variable
  [{::keys [name] :as error}]
  (format "undefined variable '%s'" (::token/lexeme name)))

(defmethod error/fmt ::unbound-variable
  [{::keys [name] :as error}]
  (format "unbound variable '%s'" (::token/lexeme name)))

(defn environment
  "Creates a new environment with no parent"
  []
  {::parent nil})

(defn push-scope
  "Returns a new environment with `environment` as its parent"
  [environment]
  {::parent environment})

(defn pop-scope
  "Returns the parent environment of `environment`"
  [environment]
  (::parent environment))

(defn- get-var*
  "Returns the value of `name` in `environment`,
  or throws an [[ExceptionInfo]] if it is unbound"
  [environment {::token/keys [lexeme] :as name}]
  (let [v (get environment lexeme)]
   (if (= v ::unbound)
     (error/runtime-error ::unbound-variable name ::name name)
     v)))

(defn get-var
  "Returns the value of variable `name` in `environment`
  Throws an [[ExceptionInfo]] if the variable is undefined or unbound"
  [environment {::token/keys [lexeme] :as name}]
  (cond
    (contains? environment lexeme) (get-var* environment name)
    (some? (::parent environment)) (get-var (::parent environment) name)
    :else (error/runtime-error ::undefined-variable name ::name name)))

(defn define-var
  "Defines a new variable `name` with value `value` in `environment`"
  [environment name value]
  (assoc environment (::token/lexeme name) value))

(defn declare-var
  "Declares an unbound variable `name` in `environment`
  Accessing it before setting it will result in an exception"
  [environment name]
  (define-var environment name ::unbound))

(defn assign-var
  "Assigns `name` to `value` in `environment`

  Throws an [[ExceptionInfo]] if the variable is undefined"
  [environment {::token/keys [lexeme] :as name} value]
  (cond
    (contains? environment lexeme) (assoc environment (::token/lexeme name) value)
    (some? (::parent environment)) (update environment ::parent assign-var name value)
    :else (error/runtime-error ::undefined-variable name ::name name)))
