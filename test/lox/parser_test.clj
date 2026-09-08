; Crafting Interpreters - Chapter 6
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.parser-test
    (:require [clojure.test :refer :all]
      [lox.scanner :as scanner]
      [lox.parser :as parser]
      [lox.ast-printer :as printer]))

;; Helper to go straight from a source string to a printed AST string
(defn- parse-to-string [source]
       (let [tokens (scanner/scan-tokens source)
             ast (parser/parse tokens)]
            (if ast
              (printer/print-ast ast)
              nil)))

(deftest test-literals
         (testing "Parses basic literals"
                  (is (= "123.0" (parse-to-string "123")))
                  (is (= "true" (parse-to-string "true")))
                  (is (= "false" (parse-to-string "false")))
                  (is (= "nil" (parse-to-string "nil")))
                  (is (= "hello" (parse-to-string "\"hello\"")))))

(deftest test-unary
         (testing "Parses unary operators"
                  (is (= "(! true)" (parse-to-string "!true")))
                  (is (= "(- 123.0)" (parse-to-string "-123")))
                  ;; Unary operators should be able to nest
                  (is (= "(! (! true))" (parse-to-string "!!true")))))

(deftest test-binary-precedence
         (testing "Enforces correct operator precedence"
                  ;; Multiplication should bind tighter than addition
                  (is (= "(+ 1.0 (* 2.0 3.0))" (parse-to-string "1 + 2 * 3")))
                  ;; Division binds tighter than subtraction
                  (is (= "(- 6.0 (/ 3.0 1.0))" (parse-to-string "6 - 3 / 1")))
                  ;; Unary binds tighter than multiplication
                  (is (= "(* (- 123.0) 45.0)" (parse-to-string "-123 * 45")))
                  ;; Comparison is lower precedence than addition
                  (is (= "(> (+ 1.0 2.0) 3.0)" (parse-to-string "1 + 2 > 3")))
                  ;; Equality is the lowest precedence
                  (is (= "(== 1.0 (< 2.0 3.0))" (parse-to-string "1 == 2 < 3")))))

(deftest test-associativity
         (testing "Enforces left-associativity for binary operators"
                  ;; 1 == 2 == 3 should parse as (1 == 2) == 3
                  (is (= "(== (== 1.0 2.0) 3.0)" (parse-to-string "1 == 2 == 3")))
                  (is (= "(- (- 5.0 3.0) 1.0)" (parse-to-string "5 - 3 - 1")))
                  (is (= "(/ (/ 8.0 4.0) 2.0)" (parse-to-string "8 / 4 / 2")))))

(deftest test-grouping
         (testing "Parentheses override standard precedence"
                  (is (= "(* (group (+ 1.0 2.0)) 3.0)" (parse-to-string "(1 + 2) * 3")))
                  (is (= "(group (group 1.0))" (parse-to-string "((1))")))
                  ;; The textbook example from Chapter 5
                  (is (= "(* (- 123.0) (group 45.67))" (parse-to-string "-123 * (45.67)")))))

(deftest test-error-handling
         (testing "Returns nil and catches errors on invalid syntax"
                  ;; Missing right parenthesis
                  (is (nil? (parse-to-string "(1 + 2")))
                  ;; Missing expression after operator
                  (is (nil? (parse-to-string "1 + ")))))