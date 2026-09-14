; Crafting Interpreters - Chapter 8
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.environment)

;; Our environment chain is a list of atoms (each atom holding a map of variables).
;; The front of the list is the innermost scope. The end of the list is the global scope.
(def ^:dynamic *env-chain* (list (atom {})))

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