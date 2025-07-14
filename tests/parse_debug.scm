(define (assert-false condition test-name)
  (if (= condition #f)
      (begin (display "PASS: ") (display test-name) (newline))
      (begin (display "FAIL: ") (display test-name) (display " - expected false") (newline))))

(assert-false #f "Minimal false test") 