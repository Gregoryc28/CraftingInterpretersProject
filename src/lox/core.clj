; Crafting Interpreters - Chapter 4
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

;; This is the main entry point for the Lox interpreter.
;; It handles command-line arguments, file reading, and REPL functionality.
(ns lox.core
    (:import [java.nio.file Files Paths]
      [java.nio.charset Charset]
      [java.io BufferedReader InputStreamReader])
    (:require [lox.scanner :as scanner]
      [lox.parser :as parser]
      [lox.ast-printer :as printer]
      [lox.error :as err]
      [lox.interpreter :as interpreter]))

;; Function to run Lox source code from a string
(defn run [source]
      ; Scan the source code into tokens, parse the tokens into an AST, and interpret the AST if no errors occurred.
      (let [tokens (scanner/scan-tokens source)
            ast (parser/parse tokens)]
           ; Check if any errors occurred during scanning or parsing before interpreting the AST
           (when-not @err/had-error
                     (when ast
                           ;; Replace the ast-printer call with our new interpreter
                           (interpreter/interpret ast)))))

; Function to run Lox source code from a file
(defn run-file [path]
      ; Read the entire file into a string, run the source code, and handle any errors that occur.
      (let [bytes (Files/readAllBytes (Paths/get path (into-array String [])))
            source (String. bytes (Charset/defaultCharset))]
           ; Run the source code and check for errors, exiting with appropriate status codes if errors occurred.
           (run source)
           (when @err/had-error (System/exit 65))
           ;; Add the runtime error exit code check
           (when @err/had-runtime-error (System/exit 70))))

;; Function to run the REPL (Read-Eval-Print Loop)
(defn run-prompt []
  ; Create a BufferedReader to read from standard input
  (let [reader (BufferedReader. (InputStreamReader. System/in))]
    ; Start an infinite loop to continuously read user input
    (loop []
      ; Print the prompt symbol to indicate readiness for input
      (print "> ")
      ; Flush the output to ensure the prompt is displayed immediately
      (flush)
      ; Read a line of input from the user
      (let [line (.readLine reader)]
        ; If the line is not nil (i.e., the user has not signaled EOF), run the line and reset the error flag
        (when line
          (run line)
          (reset! err/had-error false)
          ; Recursively call the loop function to continue the REPL
          (recur))))))

;; The main function that serves as the entry point for the program.
(defn -main [& args]
  ; Handle command-line arguments and determine whether to run a script or start the REPL
  (cond
    ; If more than one argument is provided, print usage information and exit with status code 64
    (> (count args) 1) (do (println "Usage: clj -M -m lox.core [script]") (System/exit 64))
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))