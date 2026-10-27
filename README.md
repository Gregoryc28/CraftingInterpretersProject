# Lox Interpreter
**Author:** Gregory Cohen (glcohen2@crimson.ua.edu)
**Course:** CS503

This project is a Clojure implementation of the Lox interpreter from *Crafting Interpreters*. This submission covers up to Chapter 13 (Inheritance), completing the core language interpreter.

## Directory Structure
```text
.
├── deps.edn                    # Clojure dependencies and test aliases
├── PROJECT_REQUIREMENTS.md     # Project constraints and guidelines
├── README.md                   # Project documentation
├── src/
│   └── lox/                    # Main source code for the Lox interpreter
│       ├── ast_printer.clj     # AST visualization
│       ├── callable.clj        # LoxCallable protocol definition
│       ├── core.clj            # Main entry point and REPL
│       ├── environment.clj     # Lexical environment and scope chain
│       ├── error.clj           # Error handling and reporting
│       ├── expr.clj            # Expression AST node records
│       ├── interpreter.clj     # Core tree-walk interpreter logic
│       ├── parser.clj          # Recursive descent parser
│       ├── resolver.clj        # Static analysis and variable resolution
│       ├── scanner.clj         # Lexical scanner / tokenizer
│       ├── stmt.clj            # Statement AST node records
│       └── token.clj           # Token record definitions
└── test/
    └── lox/                    # Unit testing suite
        ├── a_reporter_test.clj # Custom verbose test reporter
        ├── ast_printer_test.clj
        ├── interpreter_test.clj
        ├── parser_test.clj
        └── scanner_test.clj
```

## Prerequisites & Installation
This project is written in Clojure and uses `deps.edn`. You must have the Clojure CLI tools installed to execute the application and the test suite.

* **macOS / Linux:** `brew install clojure/tools/clojure`
* **Windows (via WSL):** See detailed instructions below.

## Windows Subsystem for Linux (WSL) Setup
Because the Clojure CLI ecosystem is heavily optimized for Unix systems, Windows users should run this project using WSL.

1. Open PowerShell as an Administrator and run:
   `wsl --install -d Ubuntu`
   *(Note: Ubuntu is the recommended distribution for this environment).*
2. Restart your computer if prompted, then open the "Ubuntu" terminal app from your Windows Start menu.
3. Update your package lists and install Java:
   `sudo apt update && sudo apt install default-jre -y`
4. Download and execute the Clojure Linux installer:
   `curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh`
   `chmod +x linux-install.sh`
   `sudo ./linux-install.sh`
5. Navigate to the project directory. Your Windows file system is automatically mounted in the `/mnt/c/` directory in WSL. For example:
   `cd /mnt/c/Users/grego/OneDrive/Desktop/CS503/LoxProject/Chapter4\ -\ Scanning/`

## Build & Run Instructions
All commands must be executed from the root of this project folder (where the `deps.edn` file is located).
(If "clj" does not work, preface each command with "clojure" instead)

**To run the interactive prompt (REPL):**
`clj -M -m lox.core`

**To run a specific Lox script file:**
`clj -M -m lox.core path/to/script.lox`

**To run the unit tests:**
`clj -M:test`

**To run the unit tests and save the results to a file:**
`clj -M:test > test_results.txt`