#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define fromspace-ptr 'uninitialized-space-ptr)
(define tospace-ptr 'uninitialized-space-ptr)

(define (init-allocator)
  (begin
    (set! heap-ptr 0)
    (set! fromspace-ptr 0)
    (set! tospace-ptr (/ (heap-size) 2))))
  
(define (gc:alloc-flat p)
  (begin
    (when (not (have-space? 2 (get-root-set)))
      (error 'gc:alloc-flat "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (not (have-space? 3 (get-root-set f r)))
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ 1 a))
      (error 'gc:first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ 2 a))
      (error 'gc:rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) r)
      (error 'set-rest! "expects address of cons")))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (if (gc:flat? a)
    (heap-ref (+ 1 a))
    (error "gc:deref: Bad type. Expected primative")))

; Contract: (gc:moved? a) -> boolean
; Purpose: checks if 'a' is pointing to something with 'moved symbol
(define (gc:moved? a)
  (eq? (heap-ref a) 'moved))

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
(define (have-space? needed-space roots)
  (if (<= needed-space (space-left))
    true
    (begin
      (stopandcopy roots)
      (<= needed-space (space-left)))))

; Contract: (space-left) -> number
; Purpose: Returns the number of slots available in the partitioned heap (what is left in the fromspace)
(define (space-left)
  (- (/ (heap-size) 2)
    (- heap-ptr fromspace-ptr)))

; Contract: (stopandcopy) -> void
; Purpose: Performs the stop and copy algorithm for this heap.
(define (stopandcopy roots)
  (begin
    (set! heap-ptr tospace-ptr)
    (let ([temp tospace-ptr])
      (begin
        (set! tospace-ptr fromspace-ptr)
        (set! fromspace-ptr temp)))
    (copyover roots)))



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
    [(gc:moved? loc)
      (heap-ref (+ 1 loc))]
    [(gc:flat? loc)
      (begin
        (when (gc:procedure? loc)
            (copyover (procedure-roots (heap-ref (+ 1 loc)))))
        (set-moved loc (gc:alloc-flat (gc:deref loc))))]
    [(gc:cons? loc)
      (set-moved loc (gc:cons (gc:first loc) (gc:rest loc)))]))

; Contract: (set-moved old-loc new-loc) -> number
; Purpose: Sets 'moved on old-loc along with new location in the next slot. Returns new-loc.
(define (set-moved old-loc new-loc)
  (begin
    (heap-set! old-loc 'moved)
    (heap-set! (+ 1 old-loc) new-loc)
    new-loc))

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

