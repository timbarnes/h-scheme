# Scheme Interpreter Test Suite

This directory contains comprehensive regression tests for the Scheme interpreter.

## Test Files

- `regression_tests.scm` - Main test suite covering all builtin functions and special forms

## Running Tests

### Option 1: Using the shell script (recommended)
```bash
./run_tests.sh
```

### Option 2: Manual execution
```bash
# Build the interpreter
cabal build

# Run the tests
cabal run scheme -- < tests/regression_tests.scm
```

### Option 3: Interactive testing
```bash
cabal run scheme
# Then load the test file: (load "tests/regression_tests.scm")
```

## Test Coverage

The test suite covers:

### Arithmetic Functions
- `+` - Addition (including variadic)
- `-` - Subtraction (unary and binary)
- `*` - Multiplication (including variadic)
- `/` - Division (unary and binary)

### Comparison Functions
- `=` - Equality
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

### List Processing Functions
- `car` - First element of list
- `cdr` - Rest of list
- `cons` - Construct pair
- `null?` - Check if empty
- `list?` - Check if list
- `length` - List length
- `append` - Concatenate lists
- `list` - Create list

### I/O Functions
- `display` - Output value
- `newline` - Output newline

### Special Forms
- `quote` - Quote expression
- `if` - Conditional
- `define` - Variable and function definition
- `lambda` - Anonymous function
- `let` - Local bindings
- `cond` - Multi-way conditional

### Advanced Features
- Recursive function definitions
- Complex nested expressions
- Error handling
- Environment management

## Test Framework

The test suite includes helper functions:

- `assert-equal expected actual test-name` - Assert equality
- `assert-true condition test-name` - Assert true condition
- `assert-false condition test-name` - Assert false condition
- `assert-error expr test-name` - Assert error condition

## Expected Output

When tests pass, you should see:
```
PASS: Test Name
```

When tests fail, you should see:
```
FAIL: Test Name - expected X, got Y
```

## Adding New Tests

To add new tests:

1. Add test cases to `regression_tests.scm`
2. Use the assertion functions for consistent output
3. Group related tests with descriptive comments
4. Test both success and error cases

## Troubleshooting

If tests fail:

1. Check that the interpreter builds successfully
2. Verify the test syntax is correct
3. Look for error messages in the output
4. Test individual expressions in the REPL to isolate issues 