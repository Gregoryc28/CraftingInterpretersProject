; Crafting Interpreters - Chapter 6
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.parser
    (:require [lox.expr :as expr]
      [lox.stmt :as stmt]
      [lox.token :as token]
      [lox.error :as err]))

; Forward declares for mutually recursive functions (avoid forward-reference errors).
(declare parse-block parse-term parse-declaration parse-assignment)

;; --- Utility Functions ---

; Reports a parse error and throws an exception to unwind the call stack.
(defn- parse-error [token message]
       (err/error (:line token) message)
       (throw (ex-info message {:type :parse-error})))

; Checks if we have reached the end of the token stream.
(defn is-at-end? [tokens]
      (= :eof (:type (first tokens))))

; Returns the current token without consuming it.
(defn peek-token [tokens]
      (first tokens))

; Checks if the current token matches the expected type.
(defn check? [tokens token-type]
      (if (is-at-end? tokens)
        false
        (= (:type (peek-token tokens)) token-type)))

;; Checks if the current token matches any of the provided types.
;; Returns [matched-token remaining-tokens] if true, or [nil tokens] if false.
(defn match [tokens & types]
      (let [current (peek-token tokens)]
           (if (some #(= (:type current) %) types)
             [current (rest tokens)]
             [nil tokens])))

;; --- Error Recovery ---

; This function is used to synchronize the parser after encountering a parse error. It consumes tokens until it finds a
; suitable point to resume parsing, such as the end of a statement or the start of a new statement.
(defn synchronize [tokens]
      ;; Consume the token that caused the error, then loop to find a recovery point
      (loop [toks (rest tokens)]
            (cond
              (is-at-end? toks) toks
              ;; If we just passed a semicolon, we are good to start the next statement
              (= (:type (first toks)) :semicolon) (rest toks)
              ;; If we see a keyword that starts a statement, stop here
              (#{:class :fun :var :for :if :while :print :return} (:type (peek-token toks))) toks
              ;; Otherwise, keep skipping
              :else (recur (rest toks)))))

;; comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
(defn parse-comparison [tokens]
      (let [[left remaining] (parse-term tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :greater :greater-equal :less :less-equal)]
                      (if operator
                        (let [[right final-toks] (parse-term next-toks)]
                             (recur (expr/->Binary expr operator right) final-toks))
                        [expr toks])))))

;; equality -> comparison ( ( "!=" | "==" ) comparison )* ;
(defn parse-equality [tokens]
      (let [[left remaining] (parse-comparison tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :bang-equal :equal-equal)]
                      (if operator
                        (let [[right final-toks] (parse-comparison next-toks)]
                             (recur (expr/->Binary expr operator right) final-toks))
                        [expr toks])))))

;; assignment -> IDENTIFIER "=" assignment | equality ;
(defn parse-assignment [tokens]
      ;; First, parse the left-hand side as a normal equality expression
      (let [[expr remaining] (parse-equality tokens)
            [eq-tok next-toks] (match remaining :equal)]
           (if eq-tok
             ;; If we found an '=', parse the right-hand side recursively (right-associative)
             (let [[value final-toks] (parse-assignment next-toks)]
                  ;; Validate that the left-hand side is a valid assignment target
                  (if (= (type expr) lox.expr.Variable)
                    [(expr/->Assign (:name expr) value) final-toks]
                    (parse-error eq-tok "Invalid assignment target.")))

             ;; If there's no '=', just return the equality expression
             [expr remaining])))

;; expression -> assignment ;
(defn parse-expression [tokens]
      ;; This used to call parse-equality, now it calls parse-assignment
      (parse-assignment tokens))

;; --- Grammar Rules ---

;; primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
(defn parse-primary [tokens]
      ;; Add :identifier to the match list
      (let [[matched next-tokens] (match tokens :false :true :nil :number :string :identifier)]
           (cond
             matched
             (let [expr-node (case (:type matched)
                                   :false (expr/->Literal false)
                                   :true  (expr/->Literal true)
                                   :nil   (expr/->Literal nil)
                                   (:number :string) (expr/->Literal (:literal matched))
                                   ;; Map the identifier token directly to our new Variable record
                                   :identifier (expr/->Variable matched))]
                  [expr-node next-tokens])

             ; If the current token is a left parenthesis, we parse a grouped expression.
             (check? tokens :left-paren)
             (let [next-tokens (rest tokens)
                   [expr after-expr-tokens] (parse-expression next-tokens)
                   [r-paren final-tokens] (match after-expr-tokens :right-paren)]
                  (if r-paren
                    [(expr/->Grouping expr) final-tokens]
                    ;; Throw error instead of returning nil
                    (parse-error (peek-token after-expr-tokens) "Expect ')' after expression.")))

             :else
             ;; Throw error instead of returning nil
             (parse-error (peek-token tokens) "Expect expression."))))

;; unary -> ( "!" | "-" ) unary | primary ;
(defn parse-unary [tokens]
      (let [[operator next-tokens] (match tokens :bang :minus)]
           (if operator
             (let [[right final-tokens] (parse-unary next-tokens)]
                  [(expr/->Unary operator right) final-tokens])
             (parse-primary tokens))))

;; factor -> unary ( ( "/" | "*" ) unary )* ;
(defn parse-factor [tokens]
      (let [[left remaining] (parse-unary tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :slash :star)]
                      (if operator
                        (let [[right final-toks] (parse-unary next-toks)]
                             (recur (expr/->Binary expr operator right) final-toks))
                        [expr toks])))))

;; term -> factor ( ( "-" | "+" ) factor )* ;
(defn parse-term [tokens]
      (let [[left remaining] (parse-factor tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :minus :plus)]
                      (if operator
                        (let [[right final-toks] (parse-factor next-toks)]
                             (recur (expr/->Binary expr operator right) final-toks))
                        [expr toks])))))

;; The main entry point for the parser.
(defn parse [tokens]
      (loop [toks tokens
             statements []]
            (if (is-at-end? toks)
              statements
              (let [[statement next-toks] (parse-declaration toks)]
                   (if statement
                     (recur next-toks (conj statements statement))
                     ;; If statement is nil (due to error recovery), continue with the synchronized tokens
                     (recur next-toks statements))))))

; Used to parse statements, which are currently limited to print statements and expression statements.
(defn parse-print-statement [tokens]
      ;; We assume the 'print' token has already been matched and stripped.
      (let [[expr remaining] (parse-expression tokens)
            [semicolon final-tokens] (match remaining :semicolon)]
           (if semicolon
             [(lox.stmt/->Print expr) final-tokens]
             (parse-error (peek-token remaining) "Expect ';' after value."))))

; Used to parse expression statements, which are expressions followed by a semicolon.
(defn parse-expression-statement [tokens]
      (let [[expr remaining] (parse-expression tokens)
            [semicolon final-tokens] (match remaining :semicolon)]
           (if semicolon
             [(lox.stmt/->Expression expr) final-tokens]
             (parse-error (peek-token remaining) "Expect ';' after expression."))))

;; block -> "{" declaration* "}" ;
(defn parse-block [tokens]
      (loop [toks tokens
             statements []]
            (cond
              ;; If we hit EOF before a closing brace, it's an error
              (is-at-end? toks) (parse-error (peek-token toks) "Expect '}' after block.")

              ;; If we find the closing brace, we are done
              (= :right-brace (:type (peek-token toks))) [(stmt/->Block statements) (rest toks)]

              ;; Otherwise, parse the next declaration
              :else (let [[statement next-toks] (parse-declaration toks)]
                         (if statement
                           (recur next-toks (conj statements statement))
                           ;; If the statement is nil (due to error recovery), continue with the synchronized tokens
                           (recur next-toks statements))))))

;; Updated to support block statements
(defn parse-statement [tokens]
      (let [[print-tok next-toks] (match tokens :print)]
           (cond
             print-tok (parse-print-statement next-toks)

             ;; Check if the statement starts with a left brace
             (= :left-brace (:type (peek-token tokens))) (parse-block (rest tokens))

             :else (parse-expression-statement tokens))))

; Used to parse variable declarations, which consist of a variable name, an optional initializer, and a terminating semicolon.
(defn parse-var-declaration [tokens]
      (let [[name-tok next-toks] (match tokens :identifier)]
           (if-not name-tok
                   (parse-error (peek-token tokens) "Expect variable name.")
                   (let [[eq-tok toks-after-eq] (match next-toks :equal)
                         ;; If there's an '=', parse the initializer expression; otherwise it's nil
                         [initializer toks-after-init] (if eq-tok
                                                         (parse-expression toks-after-eq)
                                                         [nil next-toks])
                         [semi-tok final-toks] (match toks-after-init :semicolon)]
                        (if-not semi-tok
                                (parse-error (peek-token toks-after-init) "Expect ';' after variable declaration.")
                                [(stmt/->Var name-tok initializer) final-toks])))))

; Declares a variable if the current token is 'var', otherwise it parses a statement.
; It also handles parse errors by synchronizing the parser state.
(defn parse-declaration [tokens]
      (try
        (let [[var-tok next-toks] (match tokens :var)]
             (if var-tok
               (parse-var-declaration next-toks)
               (parse-statement tokens)))
        (catch clojure.lang.ExceptionInfo e
          (if (= :parse-error (:type (ex-data e)))
            ;; If we catch a parse error, synchronize and return a nil statement
            [nil (synchronize tokens)]
            (throw e)))))