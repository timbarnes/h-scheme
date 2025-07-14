; Function definition examples
(define square (lambda (x) (* x x)))
(square 5)

(define add (lambda (x y) (+ x y)))
(add 3 4)

; Let expression example
(let ((x 5) (y 3)) (+ x y))

; Nested function calls
(square (add 2 3))

; List manipulation
(define my-list (list 1 2 3 4 5))
(car my-list)
(cdr my-list)
(length my-list) 