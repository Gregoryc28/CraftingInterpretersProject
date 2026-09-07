; Crafting Interpreters - Chapter 4
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

(ns lox.scanner
  (:require [lox.token :as token]
            [lox.error :as err]))

;; 4.7 Reserved Words
(def keywords
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

;; Helper to check if we've consumed all characters
(defn is-at-end? [{:keys [source current]}]
  (>= current (count source)))

;; Replaces the `advance()` method.
;; Returns a vector of [character-consumed updated-state-map]
(defn advance [state]
  ; Get the current character from the source string and increment the current index in the state map
  (let [c (.charAt (:source state) (:current state))]
    [c (update state :current inc)]))

;; Replaces the `addToken()` methods.
;; Takes the current state, a token type, and an optional literal.
;; Returns the updated state map with the new token added to the :tokens vector.
(defn add-token
  ([state type]
   (add-token state type nil))
  ([state type literal]
   ; Create a new token using the current state and add it to the :tokens vector in the state map
   (let [{:keys [source start current line tokens]} state
         ; Extract the lexeme (text) for the token from the source string
         text (subs source start current)
         ; Create a new token record with the specified type, lexeme, literal, and line number
         new-token (token/->Token type text literal line)]
     (update state :tokens conj new-token))))

;; Replaces the `match()` method.
;; Takes the current state and an expected character.
;; If the current character matches the expected character, it consumes it and returns true.
;; Otherwise, it returns false without consuming the character.
(defn match [state expected]
  (if (is-at-end? state)
    [false state]
    (if (= (.charAt (:source state) (:current state)) expected)
      [true (update state :current inc)]
      [false state])))

;; Replaces the `peek()` method.
;; Returns the current character without consuming it, or \null if at the end.
(defn peek-char [state]
  (if (is-at-end? state)
    ; Return a null character if we've reached the end of the source string
    \u0000
    (.charAt (:source state) (:current state))))

;; 4.6.1 String literals
;; Consumes characters until the closing quote is found or EOF is reached.
(defn scan-string [state]
  (loop [s state]
    (let [c (peek-char s)]
      (cond
        ;; If we hit EOF before a closing quote, report an error
        (is-at-end? s)
        (do
          (err/error (:line s) "Unterminated string.")
          s)

        ;; If we find the closing quote, we are done
        (= c \")
        (let [[_ s-after-quote] (advance s)
              ;; Extract the string value, trimming the surrounding quotes.
              ;; :start is the opening quote. (:current s-after-quote) is just past the closing quote.
              value (subs (:source s) (inc (:start s)) (dec (:current s-after-quote)))]
          (add-token s-after-quote :string value))

        ;; If we hit a newline, advance and increment the line counter
        (= c \newline)
        (let [[_ next-s] (advance s)]
          (recur (update next-s :line inc)))

        ;; Otherwise, just advance and keep looping
        :else
        (let [[_ next-s] (advance s)]
          (recur next-s))))))

;; 4.6.2 Number literals - Helpers

;; Replaces the `isDigit()` method.
;; Checks if the character is between '0' and '9'.
(defn is-digit? [c]
  (and (char? c) (<= (int \0) (int c) (int \9))))

;; Replaces the `peekNext()` method.
;; Returns the character one past the current one, or \u0000 if at the end.
(defn peek-next [state]
  (if (>= (inc (:current state)) (count (:source state)))
    \u0000
    (.charAt (:source state) (inc (:current state)))))

;; Replaces the `number()` method.
;; Consumes digits, an optional fractional part, and emits a :number token.
(defn scan-number [state]
  ;; First, consume all initial digits
  (let [s1 (loop [s state]
             (if (is-digit? (peek-char s))
               (let [[_ next-s] (advance s)]
                 (recur next-s))
               s))]

    ;; Then, look for a fractional part: a dot followed by another digit
    (let [s2 (if (and (= (peek-char s1) \.)
                      (is-digit? (peek-next s1)))
               ;; If found, consume the "." and loop to consume fractional digits
               (let [[_ s-after-dot] (advance s1)]
                 (loop [s s-after-dot]
                   (if (is-digit? (peek-char s))
                     (let [[_ next-s] (advance s)]
                       (recur next-s))
                     s)))

               ;; If no fractional part, just keep the state as is
               s1)]

      ;; Finally, extract the lexeme, convert it to a Double, and add the token
      (let [text (subs (:source s2) (:start s2) (:current s2))
            value (Double/parseDouble text)]
        (add-token s2 :number value)))))

;; 4.7 Identifiers - Helpers

;; Checks if the character is a-z, A-Z, or an underscore.
(defn is-alpha? [c]
  (and (char? c)
       (or (<= (int \a) (int c) (int \z))
           (<= (int \A) (int c) (int \Z))
           (= c \_))))

;; Checks if the character is alphanumeric (a-z, A-Z, 0-9, or underscore).
(defn is-alpha-numeric? [c]
  (or (is-alpha? c) (is-digit? c)))

;; Consumes alphanumeric characters and emits either a keyword or identifier token.
(defn scan-identifier [state]
  ; Consume characters until we hit a non-alphanumeric character
  (let [s-end (loop [s state]
                (if (is-alpha-numeric? (peek-char s))
                  (let [[_ next-s] (advance s)]
                    (recur next-s))
                  s))
        ; Extract the lexeme and check if it's a reserved keyword
        text (subs (:source s-end) (:start s-end) (:current s-end))
        ; Determine the token type: if it's a keyword, use its corresponding type; otherwise, it's an identifier
        type (get keywords text :identifier)]
    (add-token s-end type)))

;; Replaces the `scanToken()` method.
;; Takes the current state map, advances the character, and matches it.
;; Returns the updated state map.
(defn scan-token [state]
  (let [[c state-after-adv] (advance state)]
    (case c
      \( (add-token state-after-adv :left-paren)
      \) (add-token state-after-adv :right-paren)
      \{ (add-token state-after-adv :left-brace)
      \} (add-token state-after-adv :right-brace)
      \, (add-token state-after-adv :comma)
      \. (add-token state-after-adv :dot)
      \- (add-token state-after-adv :minus)
      \+ (add-token state-after-adv :plus)
      \; (add-token state-after-adv :semicolon)
      \* (add-token state-after-adv :star)

      ;; 4.5.2 Operators
      \! (let [[matched? next-state] (match state-after-adv \=)]
           (add-token next-state (if matched? :bang-equal :bang)))
      \= (let [[matched? next-state] (match state-after-adv \=)]
           (add-token next-state (if matched? :equal-equal :equal)))
      \< (let [[matched? next-state] (match state-after-adv \=)]
           (add-token next-state (if matched? :less-equal :less)))
      \> (let [[matched? next-state] (match state-after-adv \=)]
           (add-token next-state (if matched? :greater-equal :greater)))

      ;; 4.6 Longer Lexemes (Division and Comments)
      \/ (let [[matched? next-state] (match state-after-adv \/)]
           (if matched?
             ;; It is a comment. Consume characters until newline or EOF.
             (loop [s next-state]
               (if (or (is-at-end? s) (= (peek-char s) \newline))
                 s ;; Return the state without adding a token
                 (let [[_ adv-s] (advance s)]
                   (recur adv-s))))
             ;; It is just a division slash.
             (add-token next-state :slash)))

      ;; 4.6.1 String literals
      \" (scan-string state-after-adv)

      ;; Ignore whitespace
      \space state-after-adv
      \return state-after-adv
      \tab state-after-adv

      ;; Increment line on newline
      \newline (update state-after-adv :line inc)

      ;; Default case: Check for numbers, identifiers, or report an error for unexpected characters
      (cond
        (is-digit? c) (scan-number state-after-adv)
        (is-alpha? c) (scan-identifier state-after-adv)

        :else
        (do
          (err/error (:line state) "Unexpected character.")
          state-after-adv)))))

;; Replaces the `scanTokens()` loop.
;; We use `loop`/`recur` to act as our `while (!isAtEnd())` mechanism.
(defn scan-tokens [source]
  ; Initialize the state map with the source string, an empty tokens vector, and starting indices
  (loop [state {:source source :tokens [] :start 0 :current 0 :line 1}]
    (if (is-at-end? state)
      ;; If at the end, add the EOF token and return the tokens vector
      (conj (:tokens state) (token/->Token :eof "" nil (:line state)))

      ;; Otherwise, update :start to be equal to :current, process a token, and recurse
      (let [state-with-new-start (assoc state :start (:current state))]
        (recur (scan-token state-with-new-start))))))