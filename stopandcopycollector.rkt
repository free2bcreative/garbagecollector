#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define fromspace-ptr 'uninitialized-space-ptr)
(define tospace-ptr 'uninitialized-space-ptr)

(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! fromspace-ptr 0)
    (set! tospace-ptr (/ (heap-size) 2))))
  
; called when creating a prim (includes procedures)
(define (gc:alloc-flat p)
  (begin
    (when (not (have-space? 2))
      (error 'gc:alloc-flat "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (not (have-space? 3))
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (heap-ref (+ 1 a)))

(define (gc:rest a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (heap-set! (+ 2 a) r))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (heap-ref (+ 1 a)))

; Contract: (gc:procedure? a) -> boolean
;             a: location?
; Purpose: Checks if 'a' is gc:flat? and then if it is a procedure
(define (gc:procedure? a)
  (if (gc:flat? a)
      (procedure? (gc:deref a))
      (false)))

; Contract: (have-space? s) -> boolean
;             p : number
; Purpose: check to see if we have enough space left in the (from) heap
;   if not enough space, stop and copy is called. If still no space, return false
(define (have-space? needed-space)
  (if (<= needed-space (space-left))
    true
    (begin
      (stopandcopy)
      (<= needed-space (space-left)))))

; Contract: (space-left) -> number
; Purpose: Returns the number of slots available in the partitioned heap (what is left in the fromspace)
(define (space-left)
  (- (/ (heap-size) 2)
    (- heap-ptr fromspace-ptr)))


(define (stopandcopy)
  (println "stopandcopy processing"))
