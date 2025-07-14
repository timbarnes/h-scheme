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

(display "=== ARITHMETIC TESTS ===\n")

;; Addition tests
(assert-equal 5 (+ 2 3) "Addition: 2 + 3")
(assert-equal 0 (+ -5 5) "Addition: -5 + 5") 