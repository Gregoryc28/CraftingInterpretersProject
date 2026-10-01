; Crafting Interpreters - Chapter 7
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.interpreter
    (:require [lox.expr :as expr]
      [lox.stmt :as stmt]
      [lox.environment :as env]
      [lox.error :as err]
      [clojure.string :as str]
      [lox.callable :as callable]
      [lox.resolver :as resolver]))

;; We use a multimethod dispatching on the type of the expression record.
; This cleanly maps operations to our AST nodes without the need for a Visitor interface.
(defmulti evaluate type)

; Forward declaration for the stringify function, which converts evaluated values to strings for printing.
(declare stringify)

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

; Used to execute statements, which are currently limited to print statements and expression statements.
(defmulti execute type)

(defmethod execute lox.stmt.Expression [stmt]
           (evaluate (:expression stmt))
           nil)

(defmethod execute lox.stmt.Print [stmt]
           (let [value (evaluate (:expression stmt))]
                (println (stringify value)))
           nil)

(defmethod execute lox.stmt.Var [stmt]
           (let [value (if (:initializer stmt)
                         (evaluate (:initializer stmt))
                         nil)]
                (env/define (:lexeme (:name stmt)) value))
           nil)

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
(defn interpret [statements]
      (try
        (doseq [statement statements]
               (execute statement))
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
               (if (= :runtime-error (:type data))
                 (err/runtime-error (:token data) (ex-message e))
                 (throw e))))))

; Used to execute statements, which are currently limited to print statements and expression statements.
(defmulti execute type)

; Will execute a statement by evaluating its expression and printing the result.
; It also handles runtime errors gracefully, printing an error message if one occurs.
(defmethod execute lox.stmt.Print [stmt]
           (let [value (evaluate (:expression stmt))]
                (println (stringify value))))

; Will execute an expression statement by evaluating its expression.
(defmethod execute lox.stmt.Expression [stmt]
           (evaluate (:expression stmt))
           nil) ;; Explicitly return nil as statements don't yield values

;; --- 11.4 Interpreting Resolved Variables ---

(defmethod evaluate lox.expr.Variable [expr]
           ;; Use the exact AST record instance as the key in the IdentityHashMap
           (let [distance (.get ^java.util.IdentityHashMap @resolver/locals expr)]
                (if distance
                  (env/get-var-at distance (:lexeme (:name expr)))
                  (env/get-global (:name expr)))))

(defmethod evaluate lox.expr.Assign [expr]
           (let [value (evaluate (:value expr))
                 ; Determine if the variable is local or global based on the resolution distance stored in the IdentityHashMap
                 distance (.get ^java.util.IdentityHashMap @resolver/locals expr)]
                (if distance
                  (env/assign-at distance (:lexeme (:name expr)) value)
                  (env/assign-global (:name expr) value))
                value))

;; 8.5.2 Block Syntax and Semantics
(defmethod execute lox.stmt.Block [stmt]
           (env/with-new-environment
             (doseq [statement (:statements stmt)]
                    (execute statement)))
           nil)

;; --- Statement Execution ---

(defmethod execute lox.stmt.If [stmt]
           ; Evaluate the condition of the if statement. If it's truthy, execute the then-branch; otherwise, execute the else-branch if it exists.
           (if (truthy? (evaluate (:condition stmt)))
             (execute (:then-branch stmt))
             (when (:else-branch stmt)
                   (execute (:else-branch stmt))))
           nil)

;; --- Expression Evaluation ---

(defmethod evaluate lox.expr.Logical [expr]
           (let [left (evaluate (:left expr))
                 op-type (:type (:operator expr))]
                (cond
                  ;; For 'or', short-circuit if left is truthy
                  (and (= op-type :or) (truthy? left)) left

                  ;; For 'and', short-circuit if left is falsey
                  (and (= op-type :and) (not (truthy? left))) left

                  ;; Otherwise, evaluate and return the right operand
                  :else (evaluate (:right expr)))))

; Execute a while statement by repeatedly evaluating the condition and executing the body as long as the condition is truthy.
(defmethod execute lox.stmt.While [stmt]
           (loop []
                 (when (truthy? (evaluate (:condition stmt)))
                       (execute (:body stmt))
                       (recur)))
           nil)

;; --- 10.1 Function Calls ---
(defmethod evaluate lox.expr.Call [expr]
           (let [callee (evaluate (:callee expr))
                 arguments (mapv evaluate (:arguments expr))]
                (if-not (satisfies? callable/LoxCallable callee)
                        (throw (ex-info "Can only call functions and classes."
                                        {:type :runtime-error :token (:paren expr)}))
                        ; Check if the number of arguments matches the callable's arity. If not, throw a runtime error with the expected and actual argument counts.
                        (let [expected-arity (callable/arity callee)]
                             (if-not (= (count arguments) expected-arity)
                                     (throw (ex-info (str "Expected " expected-arity " arguments but got " (count arguments) ".")
                                                     {:type :runtime-error :token (:paren expr)}))
                                     (callable/call callee arguments))))))

;; --- 10.4 & 10.6 Function Objects with Closures ---
(defrecord LoxFunction [declaration closure]
           callable/LoxCallable
           (arity [_]
                  (count (:params declaration)))

           (call [_ arguments]
                 (let [bindings (zipmap (map :lexeme (:params declaration)) arguments)]
                      ;; Push the parameter scope onto the captured closure chain (not globals!)
                      (binding [env/*env-chain* (cons (atom bindings) closure)]
                               (try
                                 (doseq [statement (:body declaration)]
                                        (execute statement))
                                 nil ;; Implicit return nil if the end of the block is reached
                                 (catch clojure.lang.ExceptionInfo e
                                   (let [data (ex-data e)]
                                        ;; Catch our custom control-flow exception
                                        (if (= :return (:type data))
                                          (:value data)
                                          (throw e))))))))

           Object
           (toString [_]
                     (str "<fn " (:lexeme (:name declaration)) ">")))

(defmethod execute lox.stmt.Function [stmt]
           ;; Capture the current environment chain as the closure
           (let [function (->LoxFunction stmt env/*env-chain*)]
                (env/define (:lexeme (:name stmt)) function)
                nil))

;; --- 10.5 Return Statement Execution ---
(defmethod execute lox.stmt.Return [stmt]
           (let [value (if (:value stmt)
                         (evaluate (:value stmt))
                         nil)]
                ;; Throw an exception to unwind the stack out to the LoxFunction call
                (throw (ex-info "Return" {:type :return :value value}))))