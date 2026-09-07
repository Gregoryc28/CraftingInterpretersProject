; Crafting Interpreters - Chapter 5
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.ast-printer
    (:require [clojure.string :as str]
      [lox.expr :as expr]))

;; We use a multimethod dispatching on the type of the expression record.
;; This cleanly maps operations to our AST nodes without the need for a Visitor interface.

; defmulti allows us to define a multimethod, which is a function that can have different implementations based on the type of its arguments.
(defmulti print-ast type)

;; Helper function to recursively wrap expressions in parentheses
(defn- parenthesize [name & exprs]
       (str "(" name " " (str/join " " (map print-ast exprs)) ")"))

;; Implementations for each AST node type
(defmethod print-ast lox.expr.Binary [expr]
           (parenthesize (:lexeme (:operator expr)) (:left expr) (:right expr)))

(defmethod print-ast lox.expr.Grouping [expr]
           (parenthesize "group" (:expression expr)))

(defmethod print-ast lox.expr.Literal [expr]
           (if (nil? (:value expr))
             "nil"
             (str (:value expr))))

(defmethod print-ast lox.expr.Unary [expr]
           (parenthesize (:lexeme (:operator expr)) (:right expr)))