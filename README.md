# Scheme Interpreter in Haskell

A minimal but efficient Scheme interpreter implemented in Haskell, designed to be easily extensible to support the full Scheme standard.

## Features

### Core Language Support
- **Self-evaluating expressions**: numbers, strings, booleans
- **Variables**: symbol lookup and binding
- **Quoting**: `quote` and `'` syntax
- **Function application**: `(function arg1 arg2 ...)` or `[function arg1 arg2 ...]`
- **List delimiters**: Supports both parentheses `( ... )` and square brackets `[ ... ]` interchangeably for lists, function calls, and special forms
- **Special forms**: `if`, `define`, `lambda`, `let`, `cond`, `begin`, `set!`, `quote`

### Built-in Functions

#### Arithmetic
- `+`, `-`, `*`, `/` - Basic arithmetic operations
- `=`, `<`, `>`, `<=`, `>=` - Numeric comparisons

#### List Processing
- `car` - Get first element of a list
- `cdr` - Get rest of a list
- `cons` - Construct a list
- `null?` - Check if list is empty
- `list?` - Check if value is a list
- `length` - Get length of a list
- `append` - Concatenate lists
- `list` - Create a list from arguments

#### I/O
- `display` - Print a value
- `newline` - Print a newline

## Building and Running

### Prerequisites
- GHC (Glasgow Haskell Compiler) 9.0 or later
- Cabal

### Build
```bash
cabal build
```

### Run REPL
```bash
cabal run
```

### Run a Scheme file
```bash
cabal run scheme -- path/to/file.scm
```

## Usage Examples

### Basic Arithmetic
```scheme
(+ 1 2 3)           ; => 6
[* 4 5]             ; => 20
(/ 10 2)            ; => 5.0
```

### List Operations
```scheme
(cons 1 (cons 2 (cons 3 [])))  ; => (1 2 3)
(car [1 2 3])                  ; => 1
(cdr (list 1 2 3))             ; => (2 3)
(length [1 2 3])               ; => 3
```

### Conditionals
```scheme
(if (> 5 3) "yes" "no")        ; => "yes"
(if [< 5 3] "yes" "no")        ; => "no"
```

### Function Definition
```scheme
(define square (lambda (x) (* x x)))
(square 5)                      ; => 25
(define [add x y] [+ x y])
(add 2 3)                       ; => 5
```

### Let Expressions
```scheme
(let [] 42)                    ; => 42
```

### Cond Expressions
```scheme
(cond
  ([> x 0] "positive")
  ([< x 0] "negative")
  (else "zero"))
```

## Architecture

The interpreter is organized into several modules:

- **`Scheme.Core`**: Core data types and value representation
- **`Scheme.Parser`**: Lexical analysis and parsing (supports both ( ) and [ ] for lists)
- **`Scheme.Evaluator`**: Expression evaluation and function application
- **`Scheme.Environment`**: Variable binding and lookup
- **`Scheme.Builtins`**: Built-in function implementations
- **`Main`**: REPL and file execution

## Extensibility

The interpreter is designed to be easily extended:

1. **Add new special forms**: Extend the `evaluate` function in `Scheme.Evaluator`
2. **Add built-in functions**: Add to the `builtins` list in `Scheme.Builtins`
3. **Improve parsing**: Enhance the parser in `Scheme.Parser`
4. **Optimize environments**: Replace the simple list-based environment with more efficient data structures

## Testing

- The `tests/` directory contains only the regression test suite, parser tests, and required test runner files.
- Run all tests with:
  ```bash
  cabal test
  ```
- The regression suite (`regression_tests.scm`) covers all major features, including both `(`...`)` and `[`...`]` list delimiters.

## Future Enhancements

- **Macros**: Support for hygienic macros
- **Tail call optimization**: Efficient recursion
- **Garbage collection**: Memory management
- **Standard library**: Additional built-in functions
- **Error handling**: Better error messages and debugging
- **Performance optimizations**: Compilation to bytecode

## License

MIT License - see LICENSE file for details. 