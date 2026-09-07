; Crafting Interpreters - Chapter 5
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.expr)

;; In Clojure, we do not need a metaprogramming script to generate classes.
;; We can cleanly define our AST nodes using defrecord.

; defrecord is used to define a new record type in Clojure, which is similar to a class in other languages.
; Each record type represents a different kind of expression in the Lox language.

(defrecord Binary [left operator right])
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Unary [operator right])