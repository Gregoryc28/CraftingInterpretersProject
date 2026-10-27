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
      [lox.error :as err]
      [lox.resolver :as resolver]))

;; --- Helper (Updated for Chapter 11 Resolver) ---
(defn- eval-to-string [source]
       (reset! err/had-error false)
       (reset! err/had-runtime-error false)
       (reset! resolver/*scopes* ())
       (.clear ^java.util.IdentityHashMap @resolver/locals)

       ;; Print the input source code for the TA's log
       (println "\n--- Lox Source Input ---")
       (println (clojure.string/trim source))

       (let [tokens (scanner/scan-tokens source)
             stmts (parser/parse tokens)]
            (if (and stmts (not @err/had-error))
              (do
                (resolver/resolve-stmts stmts)
                (if-not @err/had-error
                        (let [result (binding [env/*env-chain* (list (atom env/default-globals))]
                                              (clojure.string/trim (with-out-str (interpreter/interpret stmts))))]

                             ;; Print the evaluated output for the TA's log
                             (println "--- Lox Console Output ---")
                             (println (if (= result "") "<no output>" result))
                             (println "--------------------------")

                             result)
                        nil))
              nil)))

;; --- Chapter 10 Tests ---

(deftest test-native-functions
         (testing "Can evaluate and execute the native clock function"
                  (is (= "<native fn>" (eval-to-string "print clock;")))

                  (let [result (eval-to-string "print clock();")]
                       ;; The updated regex (E\d+)? accounts for scientific notation formatting
                       (is (some? (re-matches #"\d+\.\d+(E\d+)?" result))))))

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

; Chapter 9 tests for control flow statements

(deftest test-if-execution
         (testing "Executes correct branch of if/else statements"
                  (is (= "1" (eval-to-string "if (true) print 1; else print 2;")))
                  (is (= "2" (eval-to-string "if (false) print 1; else print 2;")))
                  (is (= "1" (eval-to-string "if (true) print 1;")))
                  (is (= "" (eval-to-string "if (false) print 1;")))))

(deftest test-logical-execution
         (testing "Logical operators evaluate and return correct truthy/falsey values"
                  (is (= "hi" (eval-to-string "print \"hi\" or 2;")))
                  (is (= "yes" (eval-to-string "print nil or \"yes\";")))
                  (is (= "yes" (eval-to-string "print true and \"yes\";")))
                  (is (= "false" (eval-to-string "print false and \"no\";"))))

         (testing "Logical operators short-circuit execution"
                  (let [source "
      var a = \"before\";
      true or (a = \"mutated\");
      print a;"]
                       ;; The assignment should be skipped because 'true' short-circuits the 'or'
                       (is (= "before" (eval-to-string source))))

                  (let [source "
      var a = \"before\";
      false and (a = \"mutated\");
      print a;"]
                       ;; The assignment should be skipped because 'false' short-circuits the 'and'
                       (is (= "before" (eval-to-string source))))))

(deftest test-while-execution
         (testing "While loops execute repeatedly based on condition"
                  (let [source "
      var i = 0;
      while (i < 3) {
        print i;
        i = i + 1;
      }"]
                       (is (= "0\n1\n2" (eval-to-string source))))))

(deftest test-for-execution
         (testing "For loops execute initializer, condition, body, and increment"
                  (let [source "
      for (var i = 0; i < 3; i = i + 1) {
        print i;
      }"]
                       (is (= "0\n1\n2" (eval-to-string source)))))

         (testing "For loops can omit clauses safely"
                  (let [source "
      var i = 0;
      for (; i < 2;) {
        print i;
        i = i + 1;
      }"]
                       (is (= "0\n1" (eval-to-string source))))))

(deftest test-call-runtime-errors
         (testing "Catches attempts to call non-callable types"
                  (let [err-out (with-err-str (eval-to-string "\"not a function\"();"))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Can only call functions and classes."))))

         (testing "Catches attempts to call a non-callable variable"
                  (let [source "var a = 5; a();"
                        err-out (with-err-str (eval-to-string source))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Can only call functions and classes.")))))

(deftest test-user-defined-functions
         (testing "Can declare and execute a user-defined function"
                  (let [source "
      fun sayHi(first, last) {
        print \"Hi, \" + first + \" \" + last + \"!\";
      }
      sayHi(\"Dear\", \"Reader\");"]
                       (is (= "Hi, Dear Reader!" (eval-to-string source)))))

         (testing "Locally scoped parameter variables do not leak"
                  (let [source "
      fun testScope(a) {
        print a;
      }
      testScope(\"local\");
      print a;"]
                       (let [err-out (with-err-str (eval-to-string source))]
                            (is @err/had-runtime-error)
                            (is (clojure.string/includes? err-out "Undefined variable 'a'"))))))

(deftest test-returns
         (testing "Returns from a function immediately"
                  (let [source "
      fun returnIt() {
        return 5;
        print \"never reached\";
      }
      print returnIt();"]
                       (is (= "5" (eval-to-string source)))))

         (testing "Implicitly returns nil if no return statement is hit"
                  (let [source "
      fun empty() {}
      print empty();"]
                       (is (= "nil" (eval-to-string source))))))

(deftest test-recursion
         (testing "Can recursively compute fibonacci"
                  (let [source "
      fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 2) + fib(n - 1);
      }
      print fib(6);"]
                       (is (= "8" (eval-to-string source))))))

(deftest test-closures
         (testing "Functions close over their surrounding environment"
                  (let [source "
      fun makeCounter() {
        var i = 0;
        fun count() {
          i = i + 1;
          return i;
        }
        return count;
      }
      var counter = makeCounter();
      print counter(); // 1
      print counter(); // 2"]
                       (is (= "1\n2" (eval-to-string source))))))

;; --- Chapter 11 Tests ---

(deftest test-closure-lexical-scoping
         (testing "Closures capture variables from their lexical scope, not dynamic scope"
                  (let [source "
      var a = \"global\";
      {
        fun showA() {
          print a;
        }

        showA();
        var a = \"block\";
        showA();
      }"]
                       ;; Without the resolver, the second showA() would incorrectly print "block".
                       ;; With the resolver, it correctly binds to the global "a" both times.
                       (is (= "global\nglobal" (eval-to-string source))))))

;; --- Chapter 12 Tests ---

(deftest test-class-declaration-and-printing
         (testing "Declares a class and prints its runtime representation"
                  (let [source "
      class DevonshireCream {}
      print DevonshireCream;"]
                       (is (= "DevonshireCream" (eval-to-string source))))))

(deftest test-class-instantiation
         (testing "Can instantiate a class and print the instance"
                  (let [source "
      class Bagel {}
      var bagel = Bagel();
      print bagel;"]
                       (is (= "Bagel instance" (eval-to-string source))))))

(deftest test-properties
         (testing "Can get and set properties on instances"
                  (let [source "
      class Toast {}
      var toast = Toast();
      toast.jam = \"grape\";
      print toast.jam;"]
                       (is (= "grape" (eval-to-string source))))))

(deftest test-methods
         (testing "Can call methods on a class instance"
                  (let [source "
      class Bacon {
        eat() {
          print \"Crunch crunch crunch!\";
        }
      }
      Bacon().eat();"]
                       (is (= "Crunch crunch crunch!" (eval-to-string source))))))

(deftest test-this-and-constructors
         (testing "Methods can use 'this' to access instance fields"
                  (let [source "
      class Cake {
        taste() {
          print \"The \" + this.flavor + \" cake is \" + this.adjective + \"!\";
        }
      }
      var cake = Cake();
      cake.flavor = \"German chocolate\";
      cake.adjective = \"delicious\";
      cake.taste();"]
                       (is (= "The German chocolate cake is delicious!" (eval-to-string source)))))

         (testing "Constructors initialize fields correctly"
                  (let [source "
      class Point {
        init(x, y) {
          this.x = x;
          this.y = y;
        }
      }
      var p = Point(2, 3);
      print p.x;
      print p.y;"]
                       (is (= "2\n3" (eval-to-string source))))))

(deftest test-inheritance
         (testing "Subclasses inherit methods from superclasses"
                  (let [source "
      class Doughnut {
        cook() {
          print \"Fry until golden brown.\";
        }
      }
      class BostonCream < Doughnut {}
      BostonCream().cook();"]
                       (is (= "Fry until golden brown." (eval-to-string source)))))

         (testing "Catches cyclic inheritance"
                  (let [err-out (with-err-str (eval-to-string "class Oops < Oops {}"))]
                       (is @err/had-error)
                       (is (clojure.string/includes? err-out "A class can't inherit from itself."))))

         (testing "Catches inheriting from non-classes"
                  (let [err-out (with-err-str (eval-to-string "var NotClass = \"I am text\"; class Sub < NotClass {}"))]
                       (is @err/had-runtime-error)
                       (is (clojure.string/includes? err-out "Superclass must be a class.")))))

(deftest test-super-calls
         (testing "Superclass methods can be called from subclasses"
                  (let [source "
      class A {
        method() {
          print \"A method\";
        }
      }
      class B < A {
        method() {
          print \"B method\";
          super.method();
        }
      }
      class C < B {}
      C().method();"]
                       (is (= "B method\nA method" (eval-to-string source))))))