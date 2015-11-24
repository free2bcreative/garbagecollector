#lang plai/collector

; takes care of some initialization
(define (init-allocator)
  (heap-set! 0 'free)
  (heap-set! 1 (heap-size))
  (heap-set! 2 'listEnd)
  (clear-free-block 0))

; defines the pointer to the head of the free list
(define free-list
  (box 0))

; get-size number -> number
; returns the size of the data stored at the given address
(define (get-size address)
  (heap-ref (+ address 1)))

; set-side number number -> void
; sets the size in the header of this data in the heap
(define (set-size address size)
  (heap-set! (+ address 1) size))

; get-type number -> any/c
; returns the type of data stored at the given address
(define (get-type address)
  (heap-ref address))

; is-marked number -> boolean
; determines whether or not the data at this location is marked as still in scope or not
(define (is-marked address)
  (eq? (heap-ref (+ address 2)) 'marked))

; set-mark number -> void
; sets the data at this memory address as marked
(define (set-mark address)
  (heap-set! (+ address 2) 'marked))

; un-mark number -> void
; takes the mark off the data stored at the given memory address
(define (un-mark address)
  (heap-set! (+ address 2) 'unmarked))

; get-next-block number -> number
; returns the address of the block directly after and adjacent to the block at the given address
(define (get-next-block address)
  (+ address (get-size address)))

; get-next-free-block number -> number
; returns the address of the next free block of memory after the given address
(define (get-next-free-block free-block-address)
  (if (eq? free-block-address -1)
    (unbox free-list)
    (heap-ref (+ free-block-address 2))))

; set-type number any/c -> void
; sets the type of the data at the given memory address
(define (set-type address type)
  (heap-set! address type))

; set-free-next number number -> void
; sets the next pointer that teh given address has
(define (set-free-next address next-address)
  (if (eq? address -1)
      (set-box! free-list next-address)
      (heap-set! (+ address 2) next-address)))

; set-prim-val number any/c -> void
; sets the value of the prim data type at the given address
(define (set-prim-val address value)
  (heap-set! (+ address 3) value))

; set-cons-val number any/c -> void
; sets the values of the cons data type at the given address
(define (set-cons-val address first rest)
  (gc:set-first! address first)
  (gc:set-rest! address rest))

; allocate-block number (() -> list(roots)) -> void
; allocates a block of memory and returns the address that it is allocated at
(define (allocate-block block-size get-roots-func)
  (define available-memory (get-free-block block-size))
  (when (equal? -1 available-memory)
    (collect-garbage (get-roots-func))
    (set! available-memory (get-free-block block-size))
    (when (equal? -1 available-memory)
      (error 'gc:alloc-flat "OUT OF MEMORY")))
  (un-mark available-memory)
  available-memory)

; gc:alloc number -> void
; allocates a flat value. returns the address that the data was allocated at
(define (gc:alloc-flat p)
 (begin
   (define free-block (allocate-block 4 (lambda () (get-root-set))))
   (set-type free-block 'prim)
   (set-prim-val free-block p)

   free-block))
 
; gc:cons number number -> void
; allocates a cons value. returns the address that the data was allocated at
(define (gc:cons f r)
  (begin
    (define free-block (allocate-block 5 (lambda () (get-root-set f r))))
    (set-type free-block 'cons)
    (set-cons-val free-block f r)

   free-block))

; gc:cons? number -> boolean
; returns true if the value stored at this memory location is of type 'cons
(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

; gc:first number -> number
; returns the address where the first value in this list is stored
(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ a 3))
      (error 'gc:first "expects address of cons")))
 
; gc:rest number -> number
; returns the address where the rest of this list is stored without the first element.
(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ a 4))
      (error 'gc:rest "expects address of cons")))

; gc:set-first! number number -> void
; sets the pointer to the first value in the list
(define (gc:set-first! a f)
 (if (gc:cons? a)
     (heap-set! (+ a 3) f)
     (error 'set-first! "expects address of cons")))
 
; gc:set-rest! number number -> void
; sets the pointer to the rest of the values in a list
(define (gc:set-rest! a r)
 (if (gc:cons? a)
     (heap-set! (+ a 4) r)
     (error 'set-rest! "expects address of cons")))


; gc:flat? number -> boolean
; returns true if the value stored at this memory location is of type 'prim
(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

; gc:deref number -> any/c
; grabs the actual data stored at this memory address
(define (gc:deref a)
 (if (gc:flat? a)
     (heap-ref (+ a 3))
     (error 'deref! "expects address of flat")))

; get-free-block number -> number
; returns the address of the next free block that satisfies the needed size
(define (get-free-block needed-size)
  (check-next-block -1 (unbox free-list) needed-size))

; check-next-block number number number -> number
; finds a free block that is big enough to allocate a block of given size
(define (check-next-block prev-block-address current-block-address needed-size)
  (if (eq? current-block-address 'listEnd)
      -1
      (if (>= (get-size current-block-address) needed-size) ; checks if the free block is big enough
          (if (< (- (get-size current-block-address) needed-size) 3) ; check if the leftover memory is big enough for a free block
              (begin
                (set-free-next prev-block-address (get-next-free-block current-block-address)) ; set previous free block next pointer to current block's next
                current-block-address)
              (begin
                (set-size current-block-address (- (get-size current-block-address) needed-size))
                (set-size (get-next-block current-block-address) needed-size) 
                (get-next-block current-block-address)))
          (check-next-block current-block-address (get-next-free-block current-block-address) needed-size))))

; collect-garbage (list roots) -> void
; runs the mark and sweep garbage collector
(define (collect-garbage roots)
  (begin
    (map mark
       (map (lambda(x)
             (read-root x)) roots))
    (sweep -1 -1 0)))

; mark number -> void
; marks the given root-location and all objects that are in scope for this root
(define (mark root-location)
  (when (not (is-marked root-location))
    (set-mark root-location)
    (cond
      [(gc:flat? root-location)
       (when (procedure? (gc:deref root-location))
         (map mark
              (map (lambda(x)
                     (read-root x)) (procedure-roots (gc:deref root-location)))))]

      [(gc:cons? root-location)
       (begin
         (mark (gc:first root-location))
         (mark (gc:rest root-location)))])))

; should-coalesce number number -> boolean
; determines if the two blocks should be coalesced
(define (should-coalesce prevFree prev)
  (and (eq? prevFree prev) (not (eq? prevFree -1))))

; clear-free-block number -> void
; clears all the memory in this free block
(define (clear-free-block address)
  (clear-free (+ address 3) (get-next-block address)))

; clear-free number number -> void
; clears all the memory from the address start to the address end
(define (clear-free start end)
  (when (< start end)
    (begin
      (heap-set! start 'cleared)
      (clear-free (+ start 1) end)))
  end)

; sweep number number number -> void
; performs the sweep step of the garbage collection
(define (sweep prevFree prev current)
  (if (< current (heap-size))
    (if (eq? (get-type current) 'free)
        (if (should-coalesce prevFree prev) ; if prevFree = prev
            (begin
              (set-size prev (+ (get-size prev) (get-size current)))
              (sweep prevFree prev (get-next-block current)))
            (begin
              (set-free-next prevFree current)
              (sweep current current (get-next-block current)))) ; update prevFree pointer, prev, and current. make recursive call
        (if (is-marked current) ; if marked
            (begin
              (un-mark current) ; unmark
              (sweep prevFree current (get-next-block current))) ; update prev and current, make recursive call
            (begin
              (if (should-coalesce prevFree prev)
                  (begin
                    (set-size prev (+ (get-size prev) (get-size current))) ; update prev size
                    (sweep prevFree prev (clear-free current (get-next-block current)))) ; update current, make recursive call
                  (begin
                    (set-type current 'free) ; set block to be free
                    (set-free-next prevFree current)
                    (sweep current current (clear-free-block current))))))) ; update prevFree to current, prev, and current. make recursive call
    (set-free-next prevFree 'listEnd)))

; validator number number number -> void
; validates that the heap is in a good state
(define (validator prevFree prev current)
  (if (< current (heap-size))
    (cond
      [(eq? (get-type current) 'free)
       (begin 
         (cond
           [(should-coalesce prevFree prev) (error 'validator "Blocks ~s and ~s should have coalesced" prevFree current)]
           [(not (eq? (get-next-free-block prevFree) current)) (error "Previous free block ~s should point to the current block, ~s" prevFree current)])
         (validator current current (get-next-block current)))]
      [(eq? (get-type current) 'prim)
       (begin
         (cond
           [(is-marked current) (error 'validator "Block ~s is marked when it should be unmarked" current)]
           [(or (< (get-size current) 4) (> (get-size current) 7)) (error 'validator "Prim blocks should always be size four. Block ~s has size ~s." current (get-size current))])
         (validator prevFree current (get-next-block current)))]      
      [(eq? (get-type current) 'cons)
       (begin
         (cond
           [(is-marked current) (error 'validator "Block ~s is marked when it should be unmarked" current)]
           [(or (< (get-size current) 5) (> (get-size current) 8)) (error 'validator "Cons blocks should always be size five. Block ~s has size ~s." current (get-size current))])
         (validator prevFree current (get-next-block current)))]      
      [else
       (error 'validator "Block ~s has invalid type" current)])
    (when (not (eq? (get-next-free-block prevFree) 'listEnd))
      (error 'validator "End of the free list should point to 'listEnd. Instead, it's returning ~s" (get-next-free-block prevFree)))))