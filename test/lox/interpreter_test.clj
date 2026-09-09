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
      [lox.error :as err]))

;; --- Helper ---
(defn- eval-to-string [source]
       ;; Reset global error states before each run
       (reset! err/had-error false)
       (reset! err/had-runtime-error false)
       (let [tokens (scanner/scan-tokens source)
             ast (parser/parse tokens)]
            (if ast
              (str/trim (with-out-str (interpreter/interpret ast)))
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
                  (is (= "123" (eval-to-string "123")))
                  (is (= "12.34" (eval-to-string "12.34")))
                  (is (= "true" (eval-to-string "true")))
                  (is (= "false" (eval-to-string "false")))
                  (is (= "nil" (eval-to-string "nil")))
                  (is (= "hello" (eval-to-string "\"hello\"")))))

(deftest test-unary-evaluation
         (testing "Evaluates unary operators including truthiness rules"
                  (is (= "-5" (eval-to-string "-5")))
                  (is (= "false" (eval-to-string "!true")))
                  (is (= "true" (eval-to-string "!false")))
                  ;; Truthiness edge cases
                  (is (= "true" (eval-to-string "!nil")))
                  (is (= "false" (eval-to-string "!123")))
                  (is (= "false" (eval-to-string "!\"string\"")))))

(deftest test-binary-arithmetic
         (testing "Evaluates standard mathematical operations"
                  (is (= "5" (eval-to-string "2 + 3")))
                  (is (= "10" (eval-to-string "15 - 5")))
                  (is (= "24" (eval-to-string "6 * 4")))
                  (is (= "4" (eval-to-string "12 / 3")))
                  (is (= "-2.5" (eval-to-string "5 / -2")))))

(deftest test-string-concatenation
         (testing "Overloaded plus operator handles strings"
                  (is (= "helloworld" (eval-to-string "\"hello\" + \"world\"")))
                  (is (= "123456" (eval-to-string "\"123\" + \"456\"")))))

(deftest test-comparisons-and-equality
         (testing "Evaluates relational and equality operators"
                  (is (= "true" (eval-to-string "5 > 3")))
                  (is (= "false" (eval-to-string "5 < 3")))
                  (is (= "true" (eval-to-string "5 >= 5")))
                  (is (= "true" (eval-to-string "3 <= 5")))
                  (is (= "true" (eval-to-string "5 == 5")))
                  (is (= "false" (eval-to-string "5 != 5")))
                  ;; Mixed type equality
                  (is (= "false" (eval-to-string "5 == \"5\"")))
                  (is (= "true" (eval-to-string "nil == nil")))))

(deftest test-grouping
         (testing "Evaluates parenthesized expressions correctly"
                  (is (= "14" (eval-to-string "2 * (3 + 4)")))))

;; --- Runtime Error Tests ---

; Test cases to ensure that runtime errors are caught and the appropriate error flags are set, including invalid operations like negating a string or performing arithmetic with mixed types.
(deftest test-runtime-errors
         (testing "Catches invalid operations and sets error flags"
                  ;; Attempting to negate a non-number
                  (let [err-out (with-err-str (eval-to-string "-\"muffin\""))]
                       (is @err/had-runtime-error)
                       (is (str/includes? err-out "Operand must be a number.")))

                  ;; Attempting to do math with a string
                  (let [err-out (with-err-str (eval-to-string "3 - \"muffin\""))]
                       (is @err/had-runtime-error)
                       (is (str/includes? err-out "Operands must be numbers.")))

                  ;; Attempting to add mixed types
                  (let [err-out (with-err-str (eval-to-string "\"string\" + 5"))]
                       (is @err/had-runtime-error)
                       (is (str/includes? err-out "Operands must be two numbers or two strings.")))))