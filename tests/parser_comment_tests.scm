;; This is a comment at the beginning of the file
(define x 42) ; This is a comment after a definition

(define y ; Inline comment between tokens
  100)

; Comment before a list
(define z (list 1 ; comment after element
                2 ; another comment
                3))

(define (add a b) ; comment after function header
  (+ a b)) ; comment after body

; Comment before quoted
(define quoted '(1 ; comment inside quoted list
                 2 3))

; Comment at the end of the file 