; Crafting Interpreters - Chapter 8
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.environment
    (:require [lox.callable :as callable]))

;; Define the native clock function implementing LoxCallable inline
(def native-clock
  (reify
    callable/LoxCallable
    (arity [_] 0)
    (call [_ _]
          (/ (double (System/currentTimeMillis)) 1000.0))
    Object
    (toString [_] "<native fn>")))

;; Store the default global scope bindings
(def default-globals {"clock" native-clock})

;; Our environment chain is a list of atoms (each atom holding a map of variables).
;; Initialize the global scope with our native functions instead of an empty map.
(def ^:dynamic *env-chain* (list (atom default-globals)))

(defn define [name-str value]
      ;; Always define new variables in the innermost (first) scope
      (swap! (first *env-chain*) assoc name-str value))

(defn get-var [name-token]
      (let [name-str (:lexeme name-token)
            ;; Search up the chain for the first scope containing the variable
            found-env (some (fn [env-atom]
                                (when (contains? @env-atom name-str)
                                      env-atom))
                            *env-chain*)]
           (if found-env
             (get @found-env name-str)
             (throw (ex-info (str "Undefined variable '" name-str "'.")
                             {:type :runtime-error :token name-token})))))

(defn assign [name-token value]
      (let [name-str (:lexeme name-token)
            ;; Search up the chain for the variable to mutate
            found-env (some (fn [env-atom]
                                (when (contains? @env-atom name-str)
                                      env-atom))
                            *env-chain*)]
           (if found-env
             (swap! found-env assoc name-str value)
             (throw (ex-info (str "Undefined variable '" name-str "'.")
                             {:type :runtime-error :token name-token})))))

;; A macro that evaluates a block of code with a new local scope pushed onto the chain
(defmacro with-new-environment [& body]
          `(binding [*env-chain* (cons (atom {}) *env-chain*)]
                    ~@body))