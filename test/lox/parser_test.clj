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


; Chapter 9 tests for control flow statements

(deftest test-if-statement
         (testing "Parses if and else statements"
                  (let [stmts (parse-code "if (true) print 1; else print 2;")]
                       (is (= 1 (count stmts)))
                       (is (= lox.stmt.If (type (first stmts))))
                       (let [if-node (first stmts)]
                            (is (= lox.expr.Literal (type (:condition if-node))))
                            (is (= lox.stmt.Print (type (:then-branch if-node))))
                            (is (= lox.stmt.Print (type (:else-branch if-node))))))))

(deftest test-logical-operators
         (testing "Parses 'and' / 'or' with correct precedence"
                  (let [stmts (parse-code "true or false and true;")]
                       ;; 'and' should bind tighter than 'or', nesting inside the right side of the 'or'
                       (let [expr-stmt (first stmts)
                             or-node (:expression expr-stmt)]
                            (is (= lox.expr.Logical (type or-node)))
                            (is (= :or (:type (:operator or-node))))
                            (is (= lox.expr.Logical (type (:right or-node))))
                            (is (= :and (:type (:operator (:right or-node)))))))))

(deftest test-while-loop
         (testing "Parses while loops"
                  (let [stmts (parse-code "while (true) print 1;")]
                       (is (= 1 (count stmts)))
                       (is (= lox.stmt.While (type (first stmts)))))))

(deftest test-for-loop-desugaring
         (testing "For loops correctly desugar into blocks and while loops"
                  (let [stmts (parse-code "for (var i = 0; i < 1; i = i + 1) print i;")]
                       (is (= 1 (count stmts)))

                       ;; 1. The outer node is a Block containing the initializer and the While loop
                       (let [outer-block (first stmts)
                             initializer (first (:statements outer-block))
                             while-loop (second (:statements outer-block))]
                            (is (= lox.stmt.Block (type outer-block)))
                            (is (= lox.stmt.Var (type initializer)))
                            (is (= lox.stmt.While (type while-loop)))

                            ;; 2. The while loop body is a Block containing the original body and the increment
                            (let [inner-block (:body while-loop)
                                  original-body (first (:statements inner-block))
                                  increment (second (:statements inner-block))]
                                 (is (= lox.stmt.Block (type inner-block)))
                                 (is (= lox.stmt.Print (type original-body)))
                                 (is (= lox.stmt.Expression (type increment))))))))

(deftest test-function-declaration
         (testing "Parses a function declaration with parameters"
                  (let [stmts (parse-code "fun add(a, b) { print a + b; }")]
                       (is (= 1 (count stmts)))
                       (let [func-node (first stmts)]
                            (is (= lox.stmt.Function (type func-node)))
                            (is (= "add" (:lexeme (:name func-node))))
                            (is (= ["a" "b"] (map :lexeme (:params func-node))))
                            (is (= 1 (count (:body func-node))))))))

(deftest test-class-declaration
         (testing "Parses a class declaration with methods and no superclass"
                  (let [stmts (parse-code "class DevonshireCream { serveOn() { return \"Scones\"; } }")]
                       (is (= 1 (count stmts)))
                       (let [class-node (first stmts)]
                            (is (= lox.stmt.ClassDecl (type class-node)))
                            (is (= "DevonshireCream" (:lexeme (:name class-node))))
                            ;; Verify superclass is nil
                            (is (nil? (:superclass class-node)))
                            (is (= 1 (count (:methods class-node))))
                            (let [method-node (first (:methods class-node))]
                                 (is (= lox.stmt.Function (type method-node)))
                                 (is (= "serveOn" (:lexeme (:name method-node)))))))))