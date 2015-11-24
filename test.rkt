#lang plai/mutator

(allocator-setup "stopandcopycollector.rkt" 30)


;(define alist (cons 1 (cons 2 empty)))
;(define alist (cons 2 empty))
;(define extendedlist (cons 1 alist))

(define (test x)
  (lambda (a)
    (+ a x)))

(define add10 (test 10))





;(define alist (cons 1 (cons 2 empty)))


;(+ 3 4)
;(+ 5 6)
;(+ 7 8)

;(+ 1 2)
;(+ 3 4)
;(cons 1 (cons 2 (cons 3 (cons 4 empty))))

;(define a 1)
;(define b 2)
;(define c 3)
;(define d 4)

;(+ 1 2)
;(define (clos x)
;  (lambda (a) (+ a x)))
;
;(define (plus55 x)
;  ((clos 55) x))
;
;(plus55 5)
;(+ 1 (plus55 20))

;((clos 10) 20)
;((clos 30) 40)
;((clos 50) 60)

;(+ 23 d)
;(+ 30 a)
;(define e 5)
;(+ 10 12)
