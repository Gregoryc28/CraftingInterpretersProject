# CS503 Lox Interpreter (Clojure) - AI Assistant Guidelines

## Technical Workflow
* Use Clojure (via `deps.edn`) to implement the Lox interpreter, translating the textbook's Java concepts into idiomatic functional programming.
* Build directly upon the existing codebase; modify previous files rather than creating entirely new ones unless dictated by the chapter structure.
* Work through textbook chapters iteratively, section by section.
* Never generate the entire chapter's code at once; wait for confirmation before moving to the next section.
* Include the standardized file header at the top of any newly created files.

## File Header Format
; Crafting Interpreters - Chapter {number}
; Gregory Cohen
; CWID: 12440607
; Email: glcohen2@crimson.ua.edu
; CS503 - Dr. Yessick

## Testing Expectations
* Write exhaustive, unit-testing style tests for every new feature using `clojure.test`.
* Maintain the `test-runner` alias in `deps.edn` to ensure tests are runnable via the CLI.
* Cover edge cases and grammar anomalies aggressively; testing must be extensive to meet grading standards.

## Submission Requirements
* Maintain complete, terminal-based build and run instructions in the `README.md`, including WSL setup steps.
* Package the final submission as a `.zip` file containing only source code, tests, `deps.edn`, and documentation.
* Exclude all executables, IDE-specific folders (e.g., `.idea`, `.iml`), caches (e.g., `.cpcache`), and REPL artifact files.
* Explicitly document any known bugs or broken features in the README, as self-reported deficiencies incur only half the penalty.