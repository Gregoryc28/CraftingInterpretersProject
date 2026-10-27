; Crafting Interpreters - Chapter 8
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.stmt)

(defrecord Expression [expression])
(defrecord Print [expression])
(defrecord Var [name initializer])
(defrecord Block [statements])
(defrecord If [condition then-branch else-branch])
(defrecord While [condition body])
(defrecord Function [name params body])
(defrecord Return [keyword value])
(defrecord ClassDecl [name methods])