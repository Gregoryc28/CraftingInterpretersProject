; Crafting Interpreters - Chapter 11
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.resolver
    (:require [lox.expr :as expr]
      [lox.stmt :as stmt]
      [lox.error :as err])
    (:import [java.util IdentityHashMap]))

;; The scopes stack tracks local variables in the current lexical scope.
;; It is a list of maps, where each map represents a block scope.
;; The boolean value indicates whether the variable has finished its initializer.
(def ^:dynamic *scopes* (atom ()))

;; We use an IdentityHashMap to track resolution distances by the specific 
;; object instance of the expression, mirroring the Java Map<Expr, Integer>.
(def locals (atom (IdentityHashMap.)))
(def ^:dynamic *current-class* :none)
(def ^:dynamic *current-function* :none)

;; We use a multimethod dispatching on the AST node type.
(defmulti resolve-node type)

;; --- Helper Functions ---

(defn resolve-stmts [statements]
      (doseq [statement statements]
             (resolve-node statement)))

;; Scope management
(defn- begin-scope []
       (swap! *scopes* #(cons {} %)))

(defn- end-scope []
       (swap! *scopes* rest))

;; Adds the variable to the innermost scope with a false value (uninitialized)
(defn- declare-var [name-token]
       (when-not (empty? @*scopes*)
                 (let [name-str (:lexeme name-token)
                       current-scope (first @*scopes*)]
                      (when (contains? current-scope name-str)
                            (err/error (:line name-token) "Already a variable with this name in this scope."))
                      (swap! *scopes* (fn [scopes]
                                          (cons (assoc (first scopes) name-str false)
                                                (rest scopes)))))))

;; Marks the variable as fully initialized and ready for use
(defn- define-var [name-token]
       (when-not (empty? @*scopes*)
                 (let [name-str (:lexeme name-token)]
                      (swap! *scopes* (fn [scopes]
                                          (cons (assoc (first scopes) name-str true)
                                                (rest scopes)))))))

;; --- 11.3 Resolving Blocks ---

(defmethod resolve-node lox.stmt.Block [stmt]
           (begin-scope)
           (resolve-stmts (:statements stmt))
           (end-scope)
           nil)

;; --- 11.3 Resolving Variables & Assignments ---

;; Calculates the scope distance and stores it in the locals IdentityHashMap
(defn- resolve-local [expr name-token]
       (let [name-str (:lexeme name-token)
             scopes @*scopes*]
            (loop [i 0
                   remaining-scopes scopes]
                  (when-not (empty? remaining-scopes)
                            (if (contains? (first remaining-scopes) name-str)
                              ;; Match found: Store the distance (i) keyed by the exact AST object instance
                              (.put ^IdentityHashMap @locals expr i)
                              (recur (inc i) (rest remaining-scopes)))))))

(defmethod resolve-node lox.stmt.Var [stmt]
           (declare-var (:name stmt))
           (when (:initializer stmt)
                 (resolve-node (:initializer stmt)))
           (define-var (:name stmt))
           nil)

(defmethod resolve-node lox.expr.Variable [expr]
           (when-not (empty? @*scopes*)
                     (let [current-scope (first @*scopes*)
                           name-str (:lexeme (:name expr))]
                          ;; Check if the variable is declared but not yet defined
                          (when (and (contains? current-scope name-str)
                                     (= false (get current-scope name-str)))
                                (err/error (:line (:name expr)) "Can't read local variable in its own initializer."))))
           (resolve-local expr (:name expr))
           nil)

(defmethod resolve-node lox.expr.Assign [expr]
           (resolve-node (:value expr))
           (resolve-local expr (:name expr))
           nil)

;; --- 11.3 Resolving Functions ---

(defn- resolve-function [function-stmt func-type]
       (let [enclosing-function *current-function*]
            (binding [*current-function* func-type]
                     (begin-scope)
                     (doseq [param (:params function-stmt)]
                            (declare-var param)
                            (define-var param))
                     (resolve-stmts (:body function-stmt))
                     (end-scope))))

(defmethod resolve-node lox.stmt.Function [stmt]
           (declare-var (:name stmt))
           (define-var (:name stmt))
           (resolve-function stmt :function)
           nil)

(defmethod resolve-node lox.stmt.ClassDecl [stmt]
           (let [enclosing-class *current-class*]
                (binding [*current-class* :class]
                         (declare-var (:name stmt))
                         (define-var (:name stmt))

                         ;; Create a scope specifically for 'this'
                         (begin-scope)
                         (swap! *scopes* (fn [scopes]
                                             (cons (assoc (first scopes) "this" true)
                                                   (rest scopes))))

                         (doseq [method (:methods stmt)]
                                (let [declaration (if (= "init" (:lexeme (:name method)))
                                                    :initializer
                                                    :method)]
                                     (resolve-function method declaration)))

                         (end-scope)
                         nil)))

(defmethod resolve-node lox.stmt.Return [stmt]
           (when (= *current-function* :none)
                 (err/error (:line (:keyword stmt)) "Can't return from top-level code."))
           (when (:value stmt)
                 (if (= *current-function* :initializer)
                   (err/error (:line (:keyword stmt)) "Can't return a value from an initializer.")
                   (resolve-node (:value stmt))))
           nil)

(defmethod resolve-node lox.expr.This [expr]
           (if (= *current-class* :none)
             (err/error (:line (:keyword expr)) "Can't use 'this' outside of a class.")
             (resolve-local expr (:keyword expr)))
           nil)

;; --- 11.3 Resolving Other Syntax Nodes ---
;; These nodes don't interact with scopes directly; they just traverse their children.

(defmethod resolve-node lox.stmt.Expression [stmt]
           (resolve-node (:expression stmt))
           nil)

(defmethod resolve-node lox.stmt.If [stmt]
           (resolve-node (:condition stmt))
           (resolve-node (:then-branch stmt))
           (when (:else-branch stmt)
                 (resolve-node (:else-branch stmt)))
           nil)

(defmethod resolve-node lox.stmt.Print [stmt]
           (resolve-node (:expression stmt))
           nil)

(defmethod resolve-node lox.stmt.While [stmt]
           (resolve-node (:condition stmt))
           (resolve-node (:body stmt))
           nil)

(defmethod resolve-node lox.expr.Binary [expr]
           (resolve-node (:left expr))
           (resolve-node (:right expr))
           nil)

(defmethod resolve-node lox.expr.Call [expr]
           (resolve-node (:callee expr))
           (doseq [arg (:arguments expr)]
                  (resolve-node arg))
           nil)

(defmethod resolve-node lox.expr.Grouping [expr]
           (resolve-node (:expression expr))
           nil)

(defmethod resolve-node lox.expr.Literal [_expr]
           nil)

(defmethod resolve-node lox.expr.Logical [expr]
           (resolve-node (:left expr))
           (resolve-node (:right expr))
           nil)

(defmethod resolve-node lox.expr.Unary [expr]
           (resolve-node (:right expr))
           nil)

(defmethod resolve-node lox.expr.Get [expr]
           (resolve-node (:object expr))
           nil)

(defmethod resolve-node lox.expr.Set [expr]
           (resolve-node (:value expr))
           (resolve-node (:object expr))
           nil)