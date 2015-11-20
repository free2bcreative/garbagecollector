#lang plai/mutator

(allocator-setup "stopandcopycollector.rkt" 30)

;(+ 1 2)
;(+ 3 4)
;(cons 1 (cons 2 (cons 3 (cons 4 empty))))


(define a 1)
(define b 2)
(define c 3)
(define d 4)
(+ 23 d)
(+ 30 a)
;(+ 10 12)
