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

; Contract: (stopandcopy) -> void
; Purpose: Performs the stop and copy algorithm for this heap.
(define (stopandcopy)
  (begin
    (set! heap-ptr tospace-ptr)
    (let ([temp tospace-ptr])
      (begin
        (set! tospace-ptr fromspace-ptr)
        (set! fromspace-ptr temp)))
    (copyover (get-root-set))))

; Contract: (copyover l) -> void
; Purpose: Copies the items from one heap to the next one.
(define (copyover l)
  (begin
    (map (lambda (root)
      (let ([root-loc (read-root root)])
        (set-root! root (copy-single root-loc))))
      l)
    (do-cheneys fromspace-ptr)))

; Contract: (copy-single loc) -> location?
; Purpose: Copies the single item to the next heap. Returns the new location of that item
(define (copy-single loc)
  (cond
    [(gc:flat? loc)
      (begin
        (when (gc:procedure? loc)
          (copyover (procedure-roots (heap-ref (+ 1 loc)))))
        (gc:alloc-flat (gc:deref loc)))]
    [(gc:cons? loc)
      (gc:cons (gc:first loc) (gc:rest loc))]))

; Contract: (do-cheneys scan) -> (void)
; Purpose: Implements cheney's algorithm: scans through the heap for any objects still referenced in the other space.
(define (do-cheneys scan)
  (if (= scan heap-ptr)
    (void)
    (if (gc:cons? scan)
      (begin
        (heap-set! (+ 1 scan) (copy-single (heap-ref (+ 1 scan))))
        (heap-set! (+ 2 scan) (copy-single (heap-ref (+ 2 scan))))
        (do-cheneys (+ 3 scan)))
      (do-cheneys (+ 2 scan)))))

