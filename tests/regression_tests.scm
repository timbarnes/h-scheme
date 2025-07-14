;; Regression Test Suite for Scheme Interpreter
;; This file contains comprehensive tests for all builtin functions and special forms

;; Test framework helper functions
(define (assert-equal expected actual test-name)
  (if (= expected actual)
      (begin (display "PASS: ") (display test-name) (newline))
      (begin (display "FAIL: ") (display test-name) (display " - expected ") (display expected) (display ", got ") (display actual) (newline))))

(define (assert-true condition test-name)
  (if condition
      (begin (display "PASS: ") (display test-name) (newline))
      (begin (display "FAIL: ") (display test-name) (display " - expected true") (newline))))

(define (assert-false condition test-name)
  (if (= condition #f)
      (begin (display "PASS: ") (display test-name) (newline))
      (begin (display "FAIL: ") (display test-name) (display " - expected false") (newline))))

(define (assert-error expr test-name)
  (display "TEST: ") (display test-name) (display " - should produce error") (newline)
  (display "Result: ")
  (display expr)
  (newline))

;; ============================================================================
;; ARITHMETIC TESTS
;; ============================================================================

(display "=== ARITHMETIC TESTS ===\n")

;; Addition tests
(assert-equal 5 [+ 2 3] "Addition: 2 + 3 with brackets")
(assert-equal 0 (+ -5 5) "Addition: -5 + 5 with parens")
(assert-equal 15 [+ 1 2 3 4 5] "Addition: multiple arguments with brackets")
(assert-equal 0 (+) "Addition: no arguments with parens")

;; Subtraction tests
(assert-equal -1 [- 2 3] "Subtraction: 2 - 3 with brackets")
(assert-equal 5 (- 10 5) "Subtraction: 10 - 5 with parens")
(assert-equal -10 [- 5] "Subtraction: unary -5 with brackets")
(assert-equal 0 (- 10 5 5) "Subtraction: multiple arguments with parens")

;; Multiplication tests
(assert-equal 6 [* 2 3] "Multiplication: 2 * 3 with brackets")
(assert-equal 0 (* 0 5) "Multiplication: 0 * 5 with parens")
(assert-equal 120 [* 1 2 3 4 5] "Multiplication: multiple arguments with brackets")
(assert-equal 1 (*) "Multiplication: no arguments with parens")

;; Division tests
(assert-equal 2 [/ 6 3] "Division: 6 / 3 with brackets")
(assert-equal 0.5 (/ 1 2) "Division: 1 / 2 with parens")
(assert-equal 0.2 [/ 1] "Division: unary 1/5 with brackets")
(assert-equal 2 (/ 20 5 2) "Division: multiple arguments with parens")

;; ============================================================================
;; COMPARISON TESTS
;; ============================================================================

(display "\n=== COMPARISON TESTS ===\n")

;; Equality tests
(assert-true [= 5 5] "Equality: 5 = 5 with brackets")
(assert-false (= 5 6) "Equality: 5 ≠ 6 with parens")
(assert-true [= 1 1 1 1] "Equality: multiple equal numbers with brackets")
(assert-false (= 1 1 2 1) "Equality: multiple numbers with inequality with parens")

;; Less than tests
(assert-true [< 3 5] "Less than: 3 < 5 with brackets")
(assert-false (< 5 3) "Less than: 5 ≮ 3 with parens")
(assert-false [< 5 5] "Less than: 5 ≮ 5 with brackets")
(assert-true (< 1 2 3 4 5) "Less than: ascending sequence with parens")

;; Greater than tests
(assert-true [> 5 3] "Greater than: 5 > 3 with brackets")
(assert-false (> 3 5) "Greater than: 3 ≯ 5 with parens")
(assert-false [> 5 5] "Greater than: 5 ≯ 5 with brackets")
(assert-true (> 5 4 3 2 1) "Greater than: descending sequence with parens")

;; Less than or equal tests
(assert-true [<= 3 5] "Less equal: 3 ≤ 5 with brackets")
(assert-true (<= 5 5) "Less equal: 5 ≤ 5 with parens")
(assert-false [<= 5 3] "Less equal: 5 ≰ 3 with brackets")
(assert-true (<= 1 2 2 3) "Less equal: ascending with equality with parens")

;; Greater than or equal tests
(assert-true [>= 5 3] "Greater equal: 5 ≥ 3 with brackets")
(assert-true (>= 5 5) "Greater equal: 5 ≥ 5 with parens")
(assert-false [>= 3 5] "Greater equal: 3 ≱ 5 with brackets")
(assert-true (>= 5 4 4 3) "Greater equal: descending with equality with parens")

;; ============================================================================
;; LIST PROCESSING TESTS
;; ============================================================================

(display "\n=== LIST PROCESSING TESTS ===\n")

;; Cons tests
(define test-list (cons 1 (cons 2 (cons 3 []))))
(assert-equal 1 (car test-list) "Car: first element")
(assert-equal 2 (car (cdr test-list)) "Car: second element")
(assert-equal 3 (car (cdr (cdr test-list))) "Car: third element")

;; Cdr tests
(define cdr-result (cdr test-list))
(assert-equal 2 (car cdr-result) "Cdr: first element of rest")
(assert-equal 3 (car (cdr cdr-result)) "Cdr: second element of rest")
(assert-true (null? (cdr (cdr cdr-result))) "Cdr: end of list")

;; Null tests
(assert-true (null? []) "Null?: empty list with brackets")
(assert-true (null? (cdr (cdr (cdr test-list)))) "Null?: end of list")
(assert-false (null? test-list) "Null?: non-empty list")
(assert-false (null? (car test-list)) "Null?: number")

;; List tests
(assert-true (list? test-list) "List?: proper list")
(assert-true (list? []) "List?: empty list with brackets")
(assert-false (list? 5) "List?: number")
(assert-false (list? (cons 1 2)) "List?: improper list")

;; Length tests
(assert-equal 0 (length []) "Length: empty list with brackets")
(assert-equal 3 (length test-list) "Length: 3-element list")
(assert-equal 1 (length (cons 1 [])) "Length: single element with brackets")

;; Append tests
(define list1 [1 2 3])
(define list2 [4 5 6])
(define appended (append list1 list2))
(assert-equal 1 (car appended) "Append: first element")
(assert-equal 4 (car (cdr (cdr (cdr appended)))) "Append: first element of second list")
(assert-equal 6 (length appended) "Append: total length")

;; List constructor tests
(define constructed [1 2 3 4 5])
(assert-equal 5 (length constructed) "List constructor: length with brackets")
(assert-equal 1 (car constructed) "List constructor: first element")
(assert-equal 5 (car (cdr (cdr (cdr (cdr constructed))))) "List constructor: last element")

;; ============================================================================
;; I/O TESTS
;; ============================================================================

(display "\n=== I/O TESTS ===\n")

;; Display tests
(display "Display test: ")
(display 42)
(newline)

(display "Display test: ")
(display "Hello, World!")
(newline)

(display "Display test: ")
(display [1 2 3])
(newline)

;; Newline tests
(display "Newline test:")
(newline)

;; ============================================================================
;; SPECIAL FORMS TESTS
;; ============================================================================

(display "\n=== SPECIAL FORMS TESTS ===\n")

;; Quote tests
(define quoted (quote [1 2 3]))
(assert-true (list? quoted) "Quote: produces list with brackets")
(assert-equal 3 (length quoted) "Quote: preserves length")
(assert-equal 1 (car quoted) "Quote: preserves first element")

;; If tests
(assert-equal 10 (if #t 10 20) "If: true condition")
(assert-equal 20 (if #f 10 20) "If: false condition")
(assert-equal 20 (if [] 10 20) "If: nil condition with brackets")
(assert-equal 10 (if 5 10 20) "If: non-boolean true condition")

;; Define tests
(define test-var 42)
(assert-equal 42 test-var "Define: variable definition")

(define [test-func x] (* x 2))
(assert-equal 10 (test-func 5) "Define: function definition with brackets")

(define double (lambda [x] (* x 2)))
(assert-equal 8 (double 4) "Lambda: basic function with brackets")

(define add3 (lambda [x y z] [+ x y z]))
(assert-equal 6 (add3 1 2 3) "Lambda: multiple parameters and call with brackets")

;; Let tests
(define let-result (let [] 42))
(assert-equal 42 let-result "Let: simple expression with brackets")

;; Cond tests
(define cond-result (cond ([#f 1]) ([#t 2]) (else 3)))
(assert-equal 2 cond-result "Cond: true condition with brackets")

(define cond-else (cond ([#f 1]) ([#f 2]) (else 3)))
(assert-equal 3 cond-else "Cond: else clause with brackets")

;; ============================================================================
;; RECURSION TESTS
;; ============================================================================

(display "\n=== RECURSION TESTS ===\n")

;; Factorial function
(define [fact n]
  (if [= n 0]
      1
      [* n (fact [- n 1])]))

(assert-equal 1 (fact 0) "Recursion: factorial of 0 with brackets")
(assert-equal 1 (fact 1) "Recursion: factorial of 1 with brackets")
(assert-equal 120 (fact 5) "Recursion: factorial of 5 with brackets")

;; Fibonacci function
(define [fib n]
  (cond ([= n 0] 0)
        ([= n 1] 1)
        (else [+ (fib [- n 1]) (fib [- n 2])])))

(assert-equal 0 (fib 0) "Recursion: fibonacci of 0 with brackets")
(assert-equal 1 (fib 1) "Recursion: fibonacci of 1 with brackets")
(assert-equal 5 (fib 5) "Recursion: fibonacci of 5 with brackets")

;; ============================================================================
;; ERROR HANDLING TESTS
;; ============================================================================

(display "\n=== ERROR HANDLING TESTS ===\n")

;; Test division by zero
(display "Division by zero test: ")
(display (/ 1 0))
(newline)

;; Test car of empty list
(display "Car of empty list test: ")
(display (car []))
(newline)

;; Test cdr of empty list
(display "Cdr of empty list test: ")
(display (cdr []))
(newline)

;; Test wrong number of arguments
(display "Wrong number of args test: ")
(display (car 1 2))
(newline)

;; ============================================================================
;; COMPLEX EXPRESSIONS TESTS
;; ============================================================================

(display "\n=== COMPLEX EXPRESSIONS TESTS ===\n")

;; Nested function calls
(define complex-result [+ [* 2 3] [/ 10 2] [- 7 4]])
(assert-equal 12 complex-result "Complex: nested arithmetic with brackets")

;; List manipulation
(define complex-list (append [1 2] [3 4] [5 6]))
(assert-equal 6 (length complex-list) "Complex: nested append with brackets")
(assert-equal 1 (car complex-list) "Complex: first element")
(assert-equal 6 (car (cdr (cdr (cdr (cdr (cdr complex-list)))))) "Complex: last element")

;; Conditional expressions
(define conditional-result (if [> 5 3] [+ 1 2] [* 4 5]))
(assert-equal 3 conditional-result "Complex: conditional arithmetic with brackets")

;; ============================================================================
;; CLOSURE OPTIMIZATION TESTS
;; ============================================================================

(display "\n=== CLOSURE OPTIMIZATION TESTS ===\n")

;; Unused variables should not affect closure
(define unused-a 111)
(define unused-b 222)
(define used-x 7)
(define [closure-test y] [+ y used-x])
(assert-equal 10 (closure-test 3) "Closure: only referenced variable captured with brackets")

;; Changing unused variables should not affect closure
(set! unused-a 999)
(set! unused-b 888)
(assert-equal 10 (closure-test 3) "Closure: unaffected by unused variable mutation")

;; Nested closures only capture what they reference
(define outer-var 100)
(define [make-adder n]
  (lambda [m] [+ n m outer-var]))
(define add5 (make-adder 5))
(assert-equal 115 (add5 10) "Nested closure: captures outer and parameter with brackets")

;; Changing outer-var after closure creation should affect closure
(set! outer-var 200)
(assert-equal 205 (add5 0) "Nested closure: reflects updated referenced variable with brackets")

;; Recursive closure still works
(define [rec-fact n]
  (if [= n 0] 1 [* n (rec-fact [- n 1])]))
(assert-equal 120 (rec-fact 5) "Recursive closure: factorial with brackets")

(display "Closure optimization tests completed.\n")

;; ============================================================================
;; TEST SUMMARY
;; ============================================================================

(display "\n=== TEST SUMMARY ===\n")
(display "All tests completed. Check output above for PASS/FAIL results.\n")
(display "If no FAIL messages appear, all tests passed!\n") 