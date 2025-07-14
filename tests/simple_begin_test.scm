(define (assert-equal expected actual test-name)
  (if (= expected actual)
      (begin (display "PASS: ") (display test-name) (newline))
      (begin (display "FAIL: ") (display test-name) (newline))))

(assert-equal 3 (begin 1 2 3) "Begin: returns last value") 