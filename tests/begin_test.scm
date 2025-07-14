;; Begin Special Form Tests
;; This file contains comprehensive tests for the begin special form

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

(display "=== BEGIN SPECIAL FORM TESTS ===\n")

;; Basic begin tests
(assert-equal 3 (begin 1 2 3) "Begin: returns last value")
(assert-equal 42 (begin 42) "Begin: single expression")
(assert-equal () (begin) "Begin: no expressions returns nil")

;; Nested begin
(assert-equal 4 (begin 1 (begin 2 3) 4) "Begin: nested begin")

;; Begin with display (side effects)
(begin (display "A") (display "B") (display "C") (assert-equal 1 1 "Begin: display side effects"))

;; Begin with complex expressions
(define (test-begin-complex)
  (begin
    (define local-var 10)
    (* local-var 2)))

(assert-equal 20 (test-begin-complex) "Begin: complex expressions with local state")

;; Begin with conditional expressions
(define (test-begin-conditional)
  (begin
    (define flag #t)
    (if flag
        (begin (define result 100) result)
        (begin (define result 200) result))))

(assert-equal 100 (test-begin-conditional) "Begin: with conditional expressions")

;; Begin with function calls
(define (test-begin-functions)
  (begin
    (define a (+ 1 2))
    (define b (* a 3))
    (+ b 1)))

(assert-equal 10 (test-begin-functions) "Begin: with function calls")

;; Begin with list operations
(define (test-begin-lists)
  (begin
    (define lst (list 1 2 3))
    (car lst)))

(assert-equal 1 (test-begin-lists) "Begin: with list operations")

;; Begin with multiple defines
(define (test-begin-multiple-defines)
  (begin
    (define x 1)
    (define y 2)
    (define z (+ x y))
    z))

(assert-equal 3 (test-begin-multiple-defines) "Begin: multiple defines")

;; Begin with lambda definitions
(define (test-begin-lambda)
  (begin
    (define add1 (lambda (x) (+ x 1)))
    (define add2 (lambda (x) (+ x 2)))
    (add1 (add2 5))))

(assert-equal 8 (test-begin-lambda) "Begin: lambda definitions")

;; Begin with let expressions
(define (test-begin-let)
  (begin
    (define outer 10)
    (let ((x 5) (y 3))
      (+ x y outer))))

(assert-equal 18 (test-begin-let) "Begin: with let expressions")

;; Begin with cond expressions
(define (test-begin-cond)
  (begin
    (define value 7)
    (cond ((< value 5) 'small)
          ((< value 10) 'medium)
          (else 'large))))

(assert-equal 'medium (test-begin-cond) "Begin: with cond expressions")

;; Deeply nested begin
(define (test-deep-nesting)
  (begin
    (define level1 (begin
      (define level2 (begin
        (define level3 (begin
          (define level4 42)
          level4))
        level3))
      level2))
    level1))

(assert-equal 42 (test-deep-nesting) "Begin: deeply nested")

;; Begin with empty expressions (should be ignored)
(define (test-begin-empty)
  (begin
    ()
    (define x 5)
    ()
    x))

(assert-equal 5 (test-begin-empty) "Begin: with empty expressions")

;; Begin with mixed expression types
(define (test-begin-mixed)
  (begin
    (display "Starting mixed test\n")
    (define numbers (list 1 2 3 4 5))
    (define sum (+ 1 2 3 4 5))
    (define average (/ sum 5))
    (if (> average 2)
        (begin (display "Average is high\n") average)
        (begin (display "Average is low\n") 0))))

(assert-equal 3 (test-begin-mixed) "Begin: mixed expression types")

(display "\n=== BEGIN TESTS COMPLETE ===\n") 