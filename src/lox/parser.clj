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
(declare parse-term parse-assignment)
(declare parse-expression)
(declare parse-declaration)
(declare parse-statement)
(declare parse-var-declaration)
(declare parse-block)
(declare parse-or)
(declare parse-while-statement)
(declare parse-for-statement)
(declare parse-call)
(declare parse-function)

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

;; logic_and -> equality ( "and" equality )* ;
(defn parse-and [tokens]
      (let [[left remaining] (parse-equality tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :and)]
                      (if operator
                        (let [[right final-toks] (parse-equality next-toks)]
                             (recur (expr/->Logical expr operator right) final-toks))
                        [expr toks])))))

;; logic_or -> logic_and ( "or" logic_and )* ;
(defn parse-or [tokens]
      (let [[left remaining] (parse-and tokens)]
           (loop [expr left
                  toks remaining]
                 (let [[operator next-toks] (match toks :or)]
                      (if operator
                        (let [[right final-toks] (parse-and next-toks)]
                             (recur (expr/->Logical expr operator right) final-toks))
                        [expr toks])))))

;; assignment -> ( call "." )? IDENTIFIER "=" assignment | logic_or ;
(defn parse-assignment [tokens]
      (let [[expr remaining] (parse-or tokens)
            [eq-tok next-toks] (match remaining :equal)]
           (if eq-tok
             (let [[value final-toks] (parse-assignment next-toks)]
                  (cond
                    ;; Standard variable assignment
                    (= (type expr) lox.expr.Variable)
                    [(expr/->Assign (:name expr) value) final-toks]

                    ;; Property assignment (converting a Get to a Set)
                    (= (type expr) lox.expr.Get)
                    [(expr/->Set (:object expr) (:name expr) value) final-toks]

                    :else
                    (parse-error eq-tok "Invalid assignment target.")))
             [expr remaining])))

;; expression -> assignment ;
(defn parse-expression [tokens]
      ;; This used to call parse-equality, now it calls parse-assignment
      (parse-assignment tokens))

;; --- Grammar Rules ---

;; primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER | "this" | "super" "." IDENTIFIER ;
(defn parse-primary [tokens]
      (let [[matched next-tokens] (match tokens :false :true :nil :number :string :identifier :this :super)]
           (cond
             matched
             (case (:type matched)
                   :false [(expr/->Literal false) next-tokens]
                   :true  [(expr/->Literal true) next-tokens]
                   :nil   [(expr/->Literal nil) next-tokens]
                   (:number :string) [(expr/->Literal (:literal matched)) next-tokens]
                   :identifier [(expr/->Variable matched) next-tokens]
                   :this [(expr/->This matched) next-tokens]

                   ;; 13.2 Parse 'super' calls
                   :super
                   (let [[dot next-toks-d] (match next-tokens :dot)]
                        (if-not dot
                                (parse-error (peek-token next-tokens) "Expect '.' after 'super'.")
                                (let [[method final-toks] (match next-toks-d :identifier)]
                                     (if-not method
                                             (parse-error (peek-token next-toks-d) "Expect superclass method name.")
                                             [(expr/->Super matched method) final-toks])))))

             (check? tokens :left-paren)
             (let [next-tokens (rest tokens)
                   [expr after-expr-tokens] (parse-expression next-tokens)
                   [r-paren final-tokens] (match after-expr-tokens :right-paren)]
                  (if r-paren
                    [(expr/->Grouping expr) final-tokens]
                    (parse-error (peek-token after-expr-tokens) "Expect ')' after expression.")))

             :else
             (parse-error (peek-token tokens) "Expect expression."))))

; Used to parse expressions, which are currently limited to assignment and logical expressions.
(defn- finish-call [callee tokens]
       (loop [toks tokens
              args []]
             ; If the next token is a right parenthesis, we are done parsing arguments.
             (let [[r-paren no-args-toks] (match toks :right-paren)]
                  (if r-paren
                    [(expr/->Call callee r-paren args) no-args-toks]
                    (let [[arg next-toks] (parse-expression toks)
                          args-list (conj args arg)
                          _ (when (>= (count args-list) 255)
                                  ; If we have more than 255 arguments, throw a parse error.
                                  (err/error (:line (peek-token next-toks)) "Can't have more than 255 arguments."))]
                         ; If the next token is a comma, we continue parsing arguments; otherwise, we expect a right parenthesis to close the call.
                         (let [[comma after-comma] (match next-toks :comma)]
                              (if comma
                                (recur after-comma args-list)
                                (let [[r-paren final-toks] (match next-toks :right-paren)]
                                     (if r-paren
                                       [(expr/->Call callee r-paren args-list) final-toks]
                                       (parse-error (peek-token next-toks) "Expect ')' after arguments."))))))))))

; Update parse-call to handle function calls:
(defn parse-call [tokens]
      (let [[expr remaining] (parse-primary tokens)]
           (loop [callee expr
                  toks remaining]
                 (let [[l-paren next-toks-l] (match toks :left-paren)
                       [dot next-toks-d] (match toks :dot)]
                      (cond
                        ;; Handle function calls: func()
                        l-paren
                        (let [[new-callee final-toks] (finish-call callee next-toks-l)]
                             (recur new-callee final-toks))

                        ;; Handle property access: object.property
                        dot
                        (let [[name-tok final-toks] (match next-toks-d :identifier)]
                             (if name-tok
                               (recur (expr/->Get callee name-tok) final-toks)
                               (parse-error (peek-token next-toks-d) "Expect property name after '.'.")))

                        :else [callee toks])))))

; Update parse-unary to point to parse-call:
(defn parse-unary [tokens]
      (let [[operator next-tokens] (match tokens :bang :minus)]
           (if operator
             (let [[right final-tokens] (parse-unary next-tokens)]
                  [(expr/->Unary operator right) final-tokens])
             (parse-call tokens)))) ; Previously called parse-primary

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

; Parse an if statement, which consists of an 'if' keyword, a condition in parentheses, a then-branch statement, and an optional else-branch statement.
(defn parse-if-statement [tokens]
      ; We assume the 'if' token has already been matched and stripped.
      (let [[l-paren toks-after-l] (match tokens :left-paren)]
           ; Check for the opening parenthesis and parse the condition expression, then check for the closing parenthesis and parse the then-branch statement. Finally, check for an optional else-branch statement.
           (if-not l-paren (parse-error (peek-token tokens) "Expect '(' after 'if'.")
                   (let [[condition toks-after-cond] (parse-expression toks-after-l)
                         [r-paren toks-after-r] (match toks-after-cond :right-paren)]
                        ; If there's no closing parenthesis, throw a parse error. Otherwise, parse the then-branch statement and check for an optional else-branch statement.
                        (if-not r-paren (parse-error (peek-token toks-after-cond) "Expect ')' after if condition.")
                                (let [[then-branch toks-after-then] (parse-statement toks-after-r)
                                      [else-tok toks-after-else] (match toks-after-then :else)]
                                     ; If there's an 'else' token, parse the else-branch statement; otherwise, return the if statement with a nil else-branch.
                                     (if else-tok
                                       (let [[else-branch final-toks] (parse-statement toks-after-else)]
                                            [(stmt/->If condition then-branch else-branch) final-toks])
                                       [(stmt/->If condition then-branch nil) toks-after-then])))))))

(defn parse-while-statement [tokens]
      ; We assume the 'while' token has already been matched and stripped.
      (let [[l-paren toks-after-l] (match tokens :left-paren)]
           (if-not l-paren (parse-error (peek-token tokens) "Expect '(' after 'while'.")
                   (let [[condition toks-after-cond] (parse-expression toks-after-l)
                         [r-paren toks-after-r] (match toks-after-cond :right-paren)]
                        ; If there's no closing parenthesis, throw a parse error. Otherwise, parse the body statement.
                        (if-not r-paren (parse-error (peek-token toks-after-cond) "Expect ')' after condition.")
                                (let [[body final-toks] (parse-statement toks-after-r)]
                                     [(stmt/->While condition body) final-toks]))))))

(defn parse-for-statement [tokens]
      (let [[l-paren toks-after-l] (match tokens :left-paren)]
           (if-not l-paren (parse-error (peek-token tokens) "Expect '(' after 'for'.")
                   (let [;; 1. Parse Initializer
                         [initializer toks-after-init]
                         (let [[semi next-toks] (match toks-after-l :semicolon)]
                              (if semi
                                [nil next-toks]
                                ; If the next token is 'var', parse a variable declaration; otherwise, parse an expression statement.
                                (let [[var-tok next-toks-var] (match toks-after-l :var)]
                                     (if var-tok
                                       (parse-var-declaration next-toks-var)
                                       (parse-expression-statement toks-after-l)))))

                         ;; 2. Parse Condition
                         [condition toks-after-cond]
                         (let [[semi next-toks] (match toks-after-init :semicolon)]
                              (if semi
                                [nil next-toks]
                                ; If there's no semicolon, parse the condition expression and expect a semicolon after it.
                                (let [[expr next-toks-expr] (parse-expression toks-after-init)
                                      [semi2 next-toks-semi2] (match next-toks-expr :semicolon)]
                                     (if-not semi2 (parse-error (peek-token next-toks-expr) "Expect ';' after loop condition.")
                                             [expr next-toks-semi2]))))

                         ;; 3. Parse Increment
                         [increment toks-after-inc]
                         (let [[r-paren next-toks] (match toks-after-cond :right-paren)]
                              (if r-paren
                                [nil next-toks]
                                ; If there's no closing parenthesis, parse the increment expression and expect a closing parenthesis after it.
                                (let [[expr next-toks-expr] (parse-expression toks-after-cond)
                                      [r-paren2 next-toks-rparen2] (match next-toks-expr :right-paren)]
                                     (if-not r-paren2 (parse-error (peek-token next-toks-expr) "Expect ')' after for clauses.")
                                             [expr next-toks-rparen2]))))

                         ;; 4. Parse Body
                         [body final-toks] (parse-statement toks-after-inc)

                         ;; 5. Desugar into While loop and Blocks
                         body-with-inc (if increment
                                         (stmt/->Block [body (stmt/->Expression increment)])
                                         body)
                         cond-expr (or condition (expr/->Literal true))
                         while-node (stmt/->While cond-expr body-with-inc)
                         final-node (if initializer
                                      (stmt/->Block [initializer while-node])
                                      while-node)]

                        ; Return the final desugared node and the remaining tokens after parsing the for statement.
                        [final-node final-toks]))))

; Parse a function declaration, which consists of a name, parameters, and a body enclosed in braces.
(defn parse-function [kind tokens]
      (let [[name-tok toks1] (match tokens :identifier)
            _ (when-not name-tok (parse-error (peek-token tokens) (str "Expect " kind " name.")))
            [l-paren toks2] (match toks1 :left-paren)
            _ (when-not l-paren (parse-error (peek-token toks1) (str "Expect '(' after " kind " name.")))]

           ; Parse the parameters, which are a comma-separated list of identifiers, and ensure we don't exceed 255 parameters. Then parse the function body as a block statement.
           (let [parse-params (fn [start-toks]
                                  (loop [toks start-toks params []]
                                        (if (check? toks :right-paren)
                                          [params toks]
                                          (let [[param-tok t1] (match toks :identifier)
                                                _ (when-not param-tok (parse-error (peek-token toks) "Expect parameter name."))
                                                new-params (conj params param-tok)
                                                _ (when (>= (count new-params) 255)
                                                        (err/error (:line param-tok) "Can't have more than 255 parameters."))
                                                [comma t2] (match t1 :comma)]
                                               (if comma (recur t2 new-params) [new-params t1])))))

                 ; Parse the parameters and the function body, returning a Function node and the remaining tokens.
                 [params toks3] (parse-params toks2)
                 [r-paren toks4] (match toks3 :right-paren)
                 _ (when-not r-paren (parse-error (peek-token toks3) "Expect ')' after parameters."))
                 [l-brace toks5] (match toks4 :left-brace)
                 _ (when-not l-brace (parse-error (peek-token toks4) (str "Expect '{' before " kind " body.")))

                 ;; parse-block parses the statements inside the braces and returns the rest of the tokens
                 [body final-toks] (parse-block toks5)]
                [(stmt/->Function name-tok params (:statements body)) final-toks])))

; Parse a return statement, which consists of the 'return' keyword, an optional expression, and a terminating semicolon.
(defn parse-return-statement [keyword-tok tokens]
      (let [[semi-tok next-toks] (match tokens :semicolon)]
           (if semi-tok
             ;; No return value
             [(stmt/->Return keyword-tok nil) next-toks]
             ;; Has a return value
             (let [[value expr-toks] (parse-expression tokens)
                   [final-semi final-toks] (match expr-toks :semicolon)]
                  (if final-semi
                    [(stmt/->Return keyword-tok value) final-toks]
                    (parse-error (peek-token expr-toks) "Expect ';' after return value."))))))

; Update parse-statement to handle the :return token:
(defn parse-statement [tokens]
      (let [[print-tok next-toks] (match tokens :print)
            [if-tok if-next-toks] (match tokens :if)
            [while-tok while-next-toks] (match tokens :while)
            [for-tok for-next-toks] (match tokens :for)
            [return-tok return-next-toks] (match tokens :return)] ; Add return match
           (cond
             print-tok (parse-print-statement next-toks)
             if-tok (parse-if-statement if-next-toks)
             while-tok (parse-while-statement while-next-toks)
             for-tok (parse-for-statement for-next-toks)
             return-tok (parse-return-statement return-tok return-next-toks) ; Route to new parser
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

;; classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
(defn parse-class-declaration [tokens]
      (let [[name-tok toks1] (match tokens :identifier)]
           (if-not name-tok
                   (parse-error (peek-token tokens) "Expect class name.")
                   (let [[less-tok toks2] (match toks1 :less)
                         ;; If there's a '<', parse the superclass name
                         [superclass toks3] (if less-tok
                                              (let [[super-name next-toks] (match toks2 :identifier)]
                                                   (if super-name
                                                     [(expr/->Variable super-name) next-toks]
                                                     (parse-error (peek-token toks2) "Expect superclass name.")))
                                              [nil toks1])
                         [l-brace toks4] (match toks3 :left-brace)]
                        (if-not l-brace
                                (parse-error (peek-token toks3) "Expect '{' before class body.")
                                (loop [toks toks4
                                       methods []]
                                      (cond
                                        (is-at-end? toks) (parse-error (peek-token toks) "Expect '}' after class body.")

                                        (= :right-brace (:type (peek-token toks)))
                                        [(stmt/->ClassDecl name-tok superclass methods) (rest toks)]

                                        :else
                                        (let [[method next-toks] (parse-function "method" toks)]
                                             (recur next-toks (conj methods method))))))))))

; Declares a variable if the current token is 'var', otherwise it parses a statement.
; It also handles parse errors by synchronizing the parser state.
; Update parse-declaration to look for the 'fun' keyword:
(defn parse-declaration [tokens]
      (try
        (let [[class-tok toks-after-class] (match tokens :class)
              [fun-tok toks-after-fun] (match tokens :fun)
              [var-tok toks-after-var] (match tokens :var)]
             (cond
               class-tok (parse-class-declaration toks-after-class)
               fun-tok (parse-function "function" toks-after-fun)
               var-tok (parse-var-declaration toks-after-var)
               :else (parse-statement tokens)))
        (catch clojure.lang.ExceptionInfo e
          (if (= :parse-error (:type (ex-data e)))
            [nil (synchronize tokens)]
            (throw e)))))