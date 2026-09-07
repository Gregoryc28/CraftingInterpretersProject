; Crafting Interpreters - Chapter 4
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.token)

;; We use a defrecord to create a structured type for our Tokens because it provides a clear and efficient way to
;; represent the data associated with each token.
;; The `type` will be a Clojure keyword like :and, :class, :left-paren, etc.
(defrecord Token [type lexeme literal line]
  Object
  ; Override the toString method to provide a string representation of the Token
  (toString [_]
    ; Concatenate the type, lexeme, and literal into a single string for easy debugging and logging
    (str type " " lexeme " " literal)))