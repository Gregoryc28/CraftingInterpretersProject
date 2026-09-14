; Crafting Interpreters - Chapter 7
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.interpreter-test
    (:require [clojure.test :refer :all]
      [clojure.string :as str]
      [lox.scanner :as scanner]
      [lox.parser :as parser]
      [lox.interpreter :as interpreter]
      [lox.environment :as env]
      [lox.error :as err]))

;; --- Helper (Updated for Chapter 8 Statements and State) ---
(defn- eval-to-string [source]
       (reset! err/had-error false)
       (reset! err/had-runtime-error false)
       (let [tokens (scanner/scan-tokens source)
             stmts (parser/parse tokens)]
            (if (and stmts (not @err/had-error))
              ;; Bind a completely fresh environment chain for every test to prevent state leaking
              (binding [env/*env-chain* (list (atom {}))]
                       (str/trim (with-out-str (interpreter/interpret stmts))))
              nil)))

; Used to capture error output during tests
(defmacro with-err-str
          "Evaluates exprs in a context in which *err* is bound to a fresh StringWriter.
          Returns the string created by any nested printing calls."
          [& body]
          `(let [s# (java.io.StringWriter.)]
                (binding [*err* s#]
                         ~@body
                         (str s#))))

;; Testing the interpreter's evaluation of expressions and error handling

; Test cases for the interpreter, focusing on literals, unary and binary operations, string concatenation, comparisons, grouping, and runtime errors.
(deftest test-literals
         (testing "Evaluates and stringifies basic literals"
                  (is (= "123" (eval-to-string "print 123;")))
                  (is (= "12.34" (eval-to-string "print 12.34;")))
                  (is (= "true" (eval-to-string "print true;")))
                  (is (= "false" (eval-to-string "print false;")))
                  (is (= "nil" (eval-to-string "print nil;")))
                  (is (= "hello" (eval-to-string "print \"hello\";")))))

(deftest test-unary-evaluation
         (testing "Evaluates unary operators including truthiness rules"
                  (is (= "-5" (eval-to-string "print -5;")))
                  (is (= "false" (eval-to-string "print !true;")))
                  (is (= "true" (eval-to-string "print !false;")))
                  ;; Truthiness edge cases
                  (is (= "true" (eval-to-string "print !nil;")))
                  (is (= "false" (eval-to-string "print !123;")))
                  (is (= "false" (eval-to-string "print !\"string\";")))))

(deftest test-binary-arithmetic
         (testing "Evaluates standard mathematical operations"
                  (is (= "5" (eval-to-string "print 2 + 3;")))
                  (is (= "10" (eval-to-string "print 15 - 5;")))
                  (is (= "24" (eval-to-string "print 6 * 4;")))
                  (is (= "4" (eval-to-string "print 12 / 3;")))
                  (is (= "-2.5" (eval-to-string "print 5 / -2;")))))

(deftest test-string-concatenation
         (testing "Overloaded plus operator handles strings"
                  (is (= "helloworld" (eval-to-string "print \"hello\" + \"world\";")))
                  (is (= "123456" (eval-to-string "print \"123\" + \"456\";")))))

(deftest test-comparisons-and-equality
         (testing "Evaluates relational and equality operators"
                  (is (= "true" (eval-to-string "print 5 > 3;")))
                  (is (= "false" (eval-to-string "print 5 < 3;")))
                  (is (= "true" (eval-to-string "print 5 >= 5;")))
                  (is (= "true" (eval-to-string "print 3 <= 5;")))
                  (is (= "true" (eval-to-string "print 5 == 5;")))
                  (is (= "false" (eval-to-string "print 5 != 5;")))
                  ;; Mixed type equality
                  (is (= "false" (eval-to-string "print 5 == \"5\";")))
                  (is (= "true" (eval-to-string "print nil == nil;")))))

(deftest test-grouping
         (testing "Evaluates parenthesized expressions correctly"
                  (is (= "14" (eval-to-string "print 2 * (3 + 4);")))))

;; --- Runtime Error Tests ---

(deftest test-runtime-errors
         (testing "Catches invalid operations and sets error flags"
                  ;; Attempting to negate a non-number
                  (let [err-out (with-err-str (eval-to-string "print -\"muffin\";"))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Operand must be a number.")))

                  ;; Attempting to do math with a string
                  (let [err-out (with-err-str (eval-to-string "print 3 - \"muffin\";"))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Operands must be numbers.")))

                  ;; Attempting to add mixed types
                  (let [err-out (with-err-str (eval-to-string "print \"string\" + 5;"))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Operands must be two numbers or two strings.")))))

;; --- Chapter 8 Tests ---

(deftest test-print-statement
         (testing "Print statement evaluates and outputs to stdout"
                  (is (= "123" (eval-to-string "print 123;")))
                  (is (= "true" (eval-to-string "print true;")))))

(deftest test-variables-and-assignment
         (testing "Variables can be defined, accessed, and mutated"
                  (let [source "
      var a = 1;
      print a;
      a = 2;
      print a;"]
                       (is (= "1\n2" (eval-to-string source)))))

         (testing "Uninitialized variables default to nil"
                  (is (= "nil" (eval-to-string "var a; print a;")))))

(deftest test-scope-and-shadowing
         (testing "Block scopes shadow outer variables correctly"
                  (let [source "
      var a = \"global\";
      {
        var a = \"inner\";
        print a;
      }
      print a;"]
                       ;; Should print inner, then global
                       (is (= "inner\nglobal" (eval-to-string source)))))

         (testing "Inner scopes can mutate outer variables"
                  (let [source "
      var a = \"global\";
      {
        a = \"mutated\";
      }
      print a;"]
                       (is (= "mutated" (eval-to-string source))))))

(deftest test-variable-runtime-errors
         (testing "Catches undefined variable access"
                  (let [err-out (with-err-str (eval-to-string "print a;"))]
                       (is @err/had-runtime-error)
                       (is (str/includes? err-out "Undefined variable 'a'"))))

         (testing "Catches assignment to undefined variables"
                  (let [err-out (with-err-str (eval-to-string "a = 2;"))]
                       (is @err/had-runtime-error)
                       (is (str/includes? err-out "Undefined variable 'a'")))))