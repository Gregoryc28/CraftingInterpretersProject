; Crafting Interpreters - Chapter 4 Tests
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

;; Utilizes Clojure's built-in testing framework to validate the functionality of the Lox scanner.
(ns lox.scanner-test
  (:require [clojure.test :refer :all]
            [lox.scanner :as scanner]))

(defn- scan-code [source]
       (println "\n--- Scanner Source Input ---")
       (println (clojure.string/trim source))
       ;; This line must be scanner/scan-tokens so it actually calls the scanner!
       (let [tokens (scanner/scan-tokens source)]
            (println "--- Scanner Tokens Output ---")
            (doseq [tok tokens]
                   (println tok))
            (println "-----------------------------")
            tokens))

;; Test for scanning single-character tokens
(deftest single-character-tokens
  (testing "Scans basic single characters"
    (let [tokens (scan-code "(){},.-+;*")]
      ;; Check that the number of tokens is 11 (10 characters + EOF)
      (is (= 11 (count tokens))) ; 10 chars + EOF
      ;; Check that the first token is a left parenthesis
      (is (= :left-paren (:type (first tokens))))
      ;; Check that the last token is EOF
      (is (= :star (:type (nth tokens 9)))))))

;; Test for scanning two-character operators
(deftest two-character-operators
  (testing "Differentiates single vs two-character operators"
    (let [tokens (scan-code "! != = == < <= > >=")]
      (is (= [:bang :bang-equal :equal :equal-equal :less :less-equal :greater :greater-equal :eof]
             (map :type tokens))))))

;; Test for scanning string literals, including multi-line strings
(deftest string-literals
  (testing "Scans complete strings and handles multi-line"
    (let [tokens (scan-code "\"hello world\"\n\"multi\nline\"")]
      (is (= "hello world" (:literal (first tokens))))
      (is (= "multi\nline" (:literal (second tokens))))
      (is (= 3 (:line (second tokens)))))))

;; Test for scanning number literals, including integers and floating-point numbers
(deftest number-literals
  (testing "Scans integers and floating point numbers"
    (let [tokens (scan-code "123 12.34")]
      (is (= 123.0 (:literal (first tokens))))
      (is (= 12.34 (:literal (second tokens)))))))

;; Test for scanning identifiers and keywords, ensuring that reserved words are recognized correctly
(deftest identifiers-and-keywords
  (testing "Maximal munch and reserved word matching"
    (let [tokens (scan-code "var orchid = 1; if (true) {}")]
      (is (= [:var :identifier :equal :number :semicolon :if :left-paren :true :right-paren :left-brace :right-brace :eof]
             (map :type tokens))))))

;; Test for handling comments and whitespace, ensuring that comments are ignored and line numbers are tracked correctly
(deftest comments-and-whitespace
  (testing "Ignores comments and tracks lines correctly"
    (let [tokens (scan-code "// this is a comment\nvar a = 1;")]
      (is (= 6 (count tokens))) ; var, a, =, 1, ;, EOF (6 total, minus ignored)
      (is (= 2 (:line (first tokens)))))))