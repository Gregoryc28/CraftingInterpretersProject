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

;; --- Runtime Error Handling ---

(def had-runtime-error (atom false))

(defn runtime-error [token message]
      ; Prints the runtime error message along with the line number to standard error
      (binding [*out* *err*]
               (println (str message "\n[line " (:line token) "]")))
      ; Here the reset! function in clojure allows us to set the value of the atom had-runtime-error to true, indicating that a runtime error has occurred.
      (reset! had-runtime-error true))