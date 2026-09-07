; Crafting Interpreters - Chapter 5
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.ast-printer-test
    (:require [clojure.test :refer :all]
      [lox.ast-printer :refer [print-ast]]
      [lox.expr :as expr]
      [lox.token :as token]))

(deftest test-ast-printer
         (testing "Prints the AST according to the textbook example"
                  (let [;; Recreating the manual AST from the end of Chapter 5
                        expression (expr/->Binary
                                     (expr/->Unary (token/->Token :minus "-" nil 1)
                                                   (expr/->Literal 123))
                                     (token/->Token :star "*" nil 1)
                                     (expr/->Grouping (expr/->Literal 45.67)))]
                       ;; Validating it matches the expected Lisp-style string output
                       (is (= "(* (- 123) (group 45.67))" (print-ast expression))))))