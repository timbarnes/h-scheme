(define (assert-equal expected actual test-name)
  (if (= expected actual)
      (display "PASS: ") (display test-name) (newline)
      (display "FAIL: ") (display test-name) (display " - expected ") (display expected) (display ", got ") (display actual) (newline)))

(define (assert-true condition test-name)
  (if condition
      (display "PASS: ") (display test-name) (newline)
      (display "FAIL: ") (display test-name) (display " - expected true") (newline)))

(define (assert-false condition test-name)
  (if (= condition #f)
      (display "PASS: ") (display test-name) (newline)
      (display "FAIL: ") (display test-name) (display " - expected false") (newline)))

(display "=== BASIC ARITHMETIC TESTS ===\n")

(assert-equal 5 (+ 2 3) "Addition: 2 + 3")
(assert-equal 0 (+ -5 5) "Addition: -5 + 5")
(assert-equal 15 (+ 1 2 3 4 5) "Addition: multiple arguments")
(assert-equal 0 (+) "Addition: no arguments")

(assert-equal -1 (- 2 3) "Subtraction: 2 - 3")
(assert-equal 5 (- 10 5) "Subtraction: 10 - 5")
(assert-equal -10 (- 5) "Subtraction: unary -5")

(assert-equal 6 (* 2 3) "Multiplication: 2 * 3")
(assert-equal 0 (* 0 5) "Multiplication: 0 * 5")
(assert-equal 120 (* 1 2 3 4 5) "Multiplication: multiple arguments")
(assert-equal 1 (*) "Multiplication: no arguments")

(assert-equal 2 (/ 6 3) "Division: 6 / 3")
(assert-equal 0.5 (/ 1 2) "Division: 1 / 2")
(assert-equal 0.2 (/ 1) "Division: unary 1/5")

(display "\n=== BASIC COMPARISON TESTS ===\n")

(assert-true (= 5 5) "Equality: 5 = 5")
(assert-false (= 5 6) "Equality: 5 ≠ 6")
(assert-true (= 1 1 1 1) "Equality: multiple equal numbers")

(assert-true (< 3 5) "Less than: 3 < 5")
(assert-false (< 5 3) "Less than: 5 ≮ 3")
(assert-false (< 5 5) "Less than: 5 ≮ 5")

(assert-true (> 5 3) "Greater than: 5 > 3")
(assert-false (> 3 5) "Greater than: 3 ≯ 5")
(assert-false (> 5 5) "Greater than: 5 ≯ 5")

(display "\n=== BASIC LIST TESTS ===\n")

(define test-list (cons 1 (cons 2 (cons 3 ()))))
(assert-equal 1 (car test-list) "Car: first element")
(assert-equal 2 (car (cdr test-list)) "Car: second element")
(assert-equal 3 (car (cdr (cdr test-list))) "Car: third element")

(assert-true (null? ()) "Null?: empty list")
(assert-true (null? (cdr (cdr (cdr test-list)))) "Null?: end of list")
(assert-false (null? test-list) "Null?: non-empty list")

(assert-true (list? test-list) "List?: proper list")
(assert-true (list? ()) "List?: empty list")
(assert-false (list? 5) "List?: number")

(assert-equal 0 (length ()) "Length: empty list")
(assert-equal 3 (length test-list) "Length: 3-element list")

(display "\n=== BASIC SPECIAL FORMS TESTS ===\n")

(assert-equal 10 (if #t 10 20) "If: true condition")
(assert-equal 20 (if #f 10 20) "If: false condition")
(assert-equal 20 (if () 10 20) "If: nil condition")

(define test-var 42)
(assert-equal 42 test-var "Define: variable definition")

(define (test-func x) (* x 2))
(assert-equal 10 (test-func 5) "Define: function definition")

(define double (lambda (x) (* x 2)))
(assert-equal 8 (double 4) "Lambda: basic function")

(display "\n=== TEST SUMMARY ===\n")
(display "Basic tests completed. Check output above for PASS/FAIL results.\n") 