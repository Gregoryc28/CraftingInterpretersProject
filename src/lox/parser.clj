; Crafting Interpreters - Chapter 6
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.parser
    (:require [lox.expr :as expr]
      [lox.token :as token]
      [lox.error :as err]))

;; We must forward-declare the expression parser since our grammar rules 
;; are mutually recursive (e.g., primary calls expression, expression calls equality).
(declare parse-expression)

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


;; --- Grammar Rules ---

;; primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
(defn parse-primary [tokens]
      (let [[matched next-tokens] (match tokens :false :true :nil :number :string)]
           (cond
             matched
             ; If the current token is a literal (number, string, true, false, nil), we create a corresponding AST node.
             (let [expr-node (case (:type matched)
                                   :false (expr/->Literal false)
                                   :true  (expr/->Literal true)
                                   :nil   (expr/->Literal nil)
                                   (:number :string) (expr/->Literal (:literal matched)))]
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

;; expression -> equality ;
(defn parse-expression [tokens]
      (parse-equality tokens))

;; The main entry point for the parser.
(defn parse [tokens]
      (try
        ; We attempt to parse the tokens into an expression. If successful, we return the expression.
        (let [[expression _] (parse-expression tokens)]
             expression)
        ; If a parse error occurs, we catch the exception and return nil to indicate failure.
        (catch clojure.lang.ExceptionInfo e
          (if (= :parse-error (:type (ex-data e)))
            nil
            (throw e)))))