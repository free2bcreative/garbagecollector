#lang plai/mutator

(allocator-setup "stopandcopycollector.rkt" 16)

(define a 1)
(define b 2)
(define c 3)
(define d 4)
(define e 5)
(define f 6)
(define g 7)
(define h 8)
(define i 9)
(define j 10)
(define k 11)
(define l 12)
(define (blah x)
  (+ x 1))

(+ 1 2)
