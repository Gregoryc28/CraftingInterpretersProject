; Crafting Interpreters - Chapter 7
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.interpreter
    (:require [lox.expr :as expr]
      [lox.error :as err]
      [clojure.string :as str]))

;; We use a multimethod dispatching on the type of the expression record.
; This cleanly maps operations to our AST nodes without the need for a Visitor interface.
(defmulti evaluate type)

;; 7.2.1 Evaluating literals

; This method handles the evaluation of literal expressions. It simply returns the value contained in the Literal record.
(defmethod evaluate lox.expr.Literal [expr]
           (:value expr))

;; --- Helper Functions ---

;; 7.2.4 Truthiness and falsiness
;; Lox follows Ruby's rule: nil and false are falsey, everything else is truthy.
;; Clojure natively handles truthiness exactly the same way
(defn- truthy? [val]
       (boolean val))


;; --- AST Evaluation Methods ---

;; 7.2.2 Evaluating parentheses
; This method handles the evaluation of grouping expressions. It evaluates the inner expression and returns its value.
(defmethod evaluate lox.expr.Grouping [expr]
           (evaluate (:expression expr)))

;; --- 7.3.1 Runtime Error Helpers ---

; Checks if the operand is a number; if not, throws a runtime error with the operator token for context.
(defn- check-number-operand [operator operand]
       (when-not (number? operand)
                 (throw (ex-info "Operand must be a number." {:type :runtime-error :token operator}))))

; Checks if both operands are numbers; if not, throws a runtime error with the operator token for context.
(defn- check-number-operands [operator left right]
       (when-not (and (number? left) (number? right))
                 (throw (ex-info "Operands must be numbers." {:type :runtime-error :token operator}))))


;; --- AST Evaluation Methods (Updated) ---

;; 7.2.3 Evaluating unary expressions (Updated with type checks)
(defmethod evaluate lox.expr.Unary [expr]
           (let [right (evaluate (:right expr))
                 operator (:operator expr)
                 op-type (:type operator)]
                (case op-type
                      :minus (do
                               (check-number-operand operator right)
                               (- (double right)))
                      :bang  (not (truthy? right)))))

;; 7.2.5 Evaluating binary operators (Updated with type checks)
(defmethod evaluate lox.expr.Binary [expr]
           (let [left (evaluate (:left expr))
                 right (evaluate (:right expr))
                 operator (:operator expr)
                 op-type (:type operator)]
                (case op-type
                      :minus (do (check-number-operands operator left right) (- (double left) (double right)))
                      :slash (do (check-number-operands operator left right) (/ (double left) (double right)))
                      :star  (do (check-number-operands operator left right) (* (double left) (double right)))

                      :plus  (cond
                               (and (number? left) (number? right)) (+ (double left) (double right))
                               (and (string? left) (string? right)) (str left right)
                               :else (throw (ex-info "Operands must be two numbers or two strings."
                                                     {:type :runtime-error :token operator})))

                      :greater       (do (check-number-operands operator left right) (> (double left) (double right)))
                      :greater-equal (do (check-number-operands operator left right) (>= (double left) (double right)))
                      :less          (do (check-number-operands operator left right) (< (double left) (double right)))
                      :less-equal    (do (check-number-operands operator left right) (<= (double left) (double right)))

                      :bang-equal (not (= left right))
                      :equal-equal (= left right))))


;; --- 7.4 Hooking Up the Interpreter ---

; Converts the evaluated value to a string for printing, handling nil and formatting numbers without unnecessary decimal points.
(defn- stringify [val]
       (cond
         (nil? val) "nil"
         (number? val) (let [text (str val)]
                            (if (str/ends-with? text ".0")
                              (subs text 0 (- (count text) 2))
                              text))
         :else (str val)))

; This function interprets an expression by evaluating it and printing the result. It also handles runtime errors gracefully, printing an error message if one occurs.
(defn interpret [expression]
      (try
        (let [value (evaluate expression)]
             (println (stringify value)))
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
               (if (= :runtime-error (:type data))
                 (err/runtime-error (:token data) (ex-message e))
                 (throw e))))))