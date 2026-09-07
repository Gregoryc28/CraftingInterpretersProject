; Crafting Interpreters - Chapter 4
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.error)

;; Global state for error tracking
(def had-error (atom false))

;; Error reporting function that prints error messages to standard error
;; It takes the line number, an optional location string, and the error message.
(defn report [line where message]
  (binding [*out* *err*]
    (println (str "[line " line "] Error" where ": " message)))
  (reset! had-error true))

;; Convenience function for reporting errors without a specific location
(defn error [line message]
  (report line "" message))