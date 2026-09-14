; Crafting Interpreters - Chapter 8
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.parser-test-chapter-eight
    (:require [clojure.test :refer :all]
      [lox.scanner :as scanner]
      [lox.parser :as parser]
      [lox.stmt :as stmt]
      [lox.expr :as expr]
      [lox.token :as token]))

;; Helper to go straight from source to a list of parsed statements
(defn- parse-code [source]
       (let [tokens (scanner/scan-tokens source)]
            (parser/parse tokens)))

(deftest test-statement-parsing
         (testing "Parses print and expression statements"
                  (let [stmts (parse-code "print 123; 456;")]
                       (is (= 2 (count stmts)))
                       ;; Validate Print statement record
                       (is (= (stmt/->Print (expr/->Literal 123.0))
                              (first stmts)))
                       ;; Validate Expression statement record
                       (is (= (stmt/->Expression (expr/->Literal 456.0))
                              (second stmts))))))

(deftest test-variable-declaration
         (testing "Parses variable declarations with and without initializers"
                  (let [stmts (parse-code "var a = 1; var b;")]
                       (is (= 2 (count stmts)))
                       (is (= (stmt/->Var (token/->Token :identifier "a" nil 1) (expr/->Literal 1.0))
                              (first stmts)))
                       (is (= (stmt/->Var (token/->Token :identifier "b" nil 1) nil)
                              (second stmts))))))

(deftest test-assignment
         (testing "Parses assignment expressions correctly"
                  (let [stmts (parse-code "a = 2;")]
                       (is (= (stmt/->Expression (expr/->Assign (token/->Token :identifier "a" nil 1) (expr/->Literal 2.0)))
                              (first stmts))))))

(deftest test-block-parsing
         (testing "Parses block statements and nested declarations"
                  (let [stmts (parse-code "{ var a = 1; }")]
                       (is (= 1 (count stmts)))
                       (is (= (stmt/->Block [(stmt/->Var (token/->Token :identifier "a" nil 1) (expr/->Literal 1.0))])
                              (first stmts))))))

(deftest test-error-recovery
         (testing "Parser synchronizes and recovers from errors"
                  (let [stmts (parse-code "var a = ; var b = 2;")]
                       ;; The first statement throws an error and returns nil, but it should recover for 'b'
                       (is (= (stmt/->Var (token/->Token :identifier "b" nil 1) (expr/->Literal 2.0))
                              (last (remove nil? stmts)))))))