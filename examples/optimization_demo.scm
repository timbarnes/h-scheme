; Demonstration of optimized closure creation
; This example shows how the interpreter now only captures
; variables that are actually referenced by functions

; Define some variables that won't be used
(define unused1 100)
(define unused2 200)
(define unused3 300)

; Define a variable that will be used
(define multiplier 5)

; Create a function that only uses 'multiplier'
; The closure should only capture 'multiplier', not the unused variables
(define (times-multiplier x)
  (* x multiplier))

; Test the function
(display "Testing optimized closure:\n")
(display "times-multiplier 3 = ")
(display (times-multiplier 3))
(newline)

; Create another function with no free variables
(define (pure-function x y)
  (+ x y))

; Test pure function
(display "pure-function 4 5 = ")
(display (pure-function 4 5))
(newline)

; Create a nested function to show closure optimization
(define outer-var 10)
(define (outer-func x)
  (define inner-var 20)
  (lambda (y)
    (+ x y inner-var outer-var)))

; Test nested function
(define inner-func (outer-func 5))
(display "inner-func 3 = ")
(display (inner-func 3))
(newline)

(display "Optimization demo complete!\n") 