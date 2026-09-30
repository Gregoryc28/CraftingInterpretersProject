; Crafting Interpreters - Chapter 10
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.callable)

(defprotocol LoxCallable
             (arity [this] "Returns the number of arguments the callable expects.")
             (call [this arguments] "Invokes the callable with the given evaluated arguments."))