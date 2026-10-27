; Crafting Interpreters - Verbose Test Reporter
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.a-reporter-test
    (:require [clojure.test :as t]))

;; Hook into clojure.test's reporting system to print the name of each test as it runs.
;; Because this file starts with "a_", the test runner will load it first, applying these
;; overrides to all subsequent tests.
(defmethod t/report :begin-test-var [m]
           (t/with-test-out
             (println "\n==================================================")
             (println "Running Test:" (-> m :var meta :name))))

;; Override the default pass behavior to print a confirmation for each passed assertion.
;; We must call inc-report-counter so the final summary at the end remains accurate.
(defmethod t/report :pass [m]
           (t/with-test-out
             (t/inc-report-counter :pass)
             (println "  [✔ PASS] Assertion successful")))

;; The default clojure.test implementations for :fail and :error are already highly verbose,
;; so we don't need to override them