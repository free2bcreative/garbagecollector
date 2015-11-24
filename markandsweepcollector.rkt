#lang plai/collector

(define (init-allocator)
  (heap-set! 0 'free)
  (heap-set! 1 (heap-size))
  (heap-set! 2 'listEnd))

(define free-list
  (box 0))
  
(define (gc:alloc-flat p)
 (begin
   (define available-memory (get-free-block 4))
   (when (equal? -1 available-memory)
     (define root-set (get-root-set))
     (collect-garbage root-set)
     (set! available-memory (get-free-block 4))
     (when (equal? -1 available-memory)
       (error 'gc:alloc-flat "FLAT - OUT OF MEMORY")))
   (heap-set! available-memory 'prim)
   (heap-set! (+ 2 available-memory) p)

   available-memory))
 
(define (gc:cons f r)
  (begin
   (define available-memory (get-free-block 5))
   (when (equal? -1 available-memory)
     (define root-set (get-root-set f r))
     (collect-garbage root-set)
     (set! available-memory (get-free-block 5))
     (when (equal? -1 available-memory)
       (error 'gc:alloc-flat "CONS - OUT OF MEMORY")))
   (heap-set! available-memory 'cons)
   (heap-set! (+ 2 available-memory) f)
   (heap-set! (+ 3 available-memory) r)

   available-memory))
 
(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))
 
(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ 2 a))
      (error 'gc:first "expects address of cons")))
 
(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ 3 a))
      (error 'gc:rest "expects address of cons")))
 
(define (gc:set-first! a f)
 (if (gc:cons? a)
     (heap-set! (+ 2 a) f)
     (error 'set-first! "expects address of cons")))
 
(define (gc:set-rest! a r)
 (if (gc:cons? a)
     (heap-set! (+ 3 a) r)
     (error 'set-rest! "expects address of cons")))
 
(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))
 
(define (gc:deref a)
 (if (gc:flat? a)
     (heap-ref (+ 2 a))
     (error 'deref! "expects address of flat")))

(define (get-free-block needed-size)
  (begin
    (define head (unbox free-list))    
    (if (eq? head 'listEnd) ; checks if we're at the end of the list
        -1 ; if so, returns an error
        (if (>= (heap-ref (+ 1 head)) needed-size) ; checks if there is enough free space
            (if (< (- (heap-ref (+ 1 head)) needed-size) 3) ; checks if there is enough leftover space for another free block
                (begin ; if not
                  (set-box! free-list (heap-ref (+ 2 head))) ; set free list to now point to the new head
                  head) ; returns the address where new memory is to be allocated
                (begin
                  (heap-set! (+ head needed-size) 'free) ; set new free block header - type
                  (heap-set! (+ head needed-size 1) (- (heap-ref (+ head 1)) needed-size)) ; set new free block header - size
                  (heap-set! (+ head needed-size 2) (heap-ref (+ head 2))) ; set new free block header - next
                  (heap-set! (+ head 1) needed-size) ; set now allocated block - size
                  (set-box! free-list (+ head needed-size)) ; set free list to now point to the new head
                  head)) ; returns the address where new memory is to be allocated
            (check-next-block head (heap-ref (+ 2 head)) needed-size))))) ; makes the recursive call
  
; check-next-block number number number -> boolean
; determines if there is a free block that is big enough to allocate a block of given size
(define (check-next-block prev-block-address current-block-address needed-size)
  (if (eq? current-block-address 'listEnd)
      -1
      (if (>= (heap-ref (+ 1 current-block-address)) needed-size) ; checks if the free block is big enough
          (if (< (- (heap-ref (+ 1 current-block-address)) needed-size) 3) ; check if the leftover memory is big enough for a free block
              (begin
                (heap-set! (+ 2 prev-block-address) (heap-ref (+ 2 current-block-address))) ; set previous free block next pointer to current block's next
                current-block-address)
              (begin
                (heap-set! (+ current-block-address needed-size) 'free) ; set new free block header - type
                (heap-set! (+ current-block-address needed-size 1) (- (heap-ref (+ current-block-address 1)) needed-size)) ; set new free block header -size
                (heap-set! (+ current-block-address needed-size 2) (heap-ref (+ current-block-address 2))) ; set new free block header - next
                (heap-set! (+ current-block-address 1) needed-size) ; set now allocated block - size
                (heap-set! (+ prev-block-address 2) (+ current-block-address needed-size))
                current-block-address))
          (check-next-block current-block-address (heap-ref (+ 2 current-block-address)) needed-size))))
  
(define (collect-garbage roots)
  (begin
    (unmark 0)
    (map mark
       (map (lambda(x)
             (read-root x)) roots))
    (sweep)))

(define (unmark next)
  (when (< next (heap-size))
    (cond
      [(eq? (heap-ref next) 'free) (unmark (+ next (heap-ref (+ next 1))))]
      [(gc:flat? next)
       (begin
         (heap-set! (+ next 3) 'unmarked)
         (unmark (+ next (heap-ref (+ next 1)))))]
      [(gc:cons? next)
       (begin
         (heap-set! (+ next 4) 'unmarked)
         (unmark (+ next (heap-ref (+ next 1)))))])))
  
(define (mark root-location)
  (cond
    [(gc:flat? root-location)
     (if (procedure? root-location)
         (if (eq? (heap-ref (+ root-location 3)) 'marked)
             (heap-set! (+ root-location 3) 'marked)
             (begin
               (map mark
                    (map (lambda(x)
                           (read-root x)) (procedure-roots root-location)))
               (heap-set! (+ root-location 3) 'marked)))
             (heap-set! (+ root-location 3) 'marked))]
    [(gc:cons? root-location)
     (begin
       (if (eq? (heap-ref (+ root-location 4)) 'marked)
           (heap-set! (+ root-location 4) 'marked)
           (begin
             (heap-set! (+ root-location 4) 'marked)
             (mark (heap-ref (+ root-location 2)))
             (mark (heap-ref (+ root-location 3))))))]))

(define (sweep)
  (cond
    [(eq? (heap-ref 0) 'free) (sweep-helper 0 (heap-ref 1))]
    [(gc:flat? 0)
     (if (eq? (heap-ref 3) 'marked)
         (sweep-helper 0 (heap-ref 1))
         (begin
           (heap-set! 0 'free)
           (heap-set! 2 (unbox free-list))
           (set-box! free-list 0)
           (sweep-helper 0 (heap-ref 1))))]
    [(gc:cons? 0)
     (if (eq? (heap-ref 4) 'marked)
         (sweep-helper 0 (heap-ref 1))
         (begin
           (heap-set! 0 'free)
           (heap-set! 2 (unbox free-list))
           (set-box! free-list 0)
           (sweep-helper 0 (heap-ref 1))))]))

(define (sweep-helper prev next)
  (when (< next (heap-size))
    (cond
      [(eq? (heap-ref next) 'free) (sweep-helper next (+ next (heap-ref (+ next 1))))]
      [(gc:flat? next)
       (if (eq? (heap-ref (+ next 3)) 'marked)
           (sweep-helper next (+ next (heap-ref (+ next 1))))
           (begin
             (heap-set! next 'free)
             (heap-set! (+ next 2) (unbox free-list))
             (set-box! free-list next)
             
             (sweep-helper next (+ next (heap-ref (+ next 1))))))]
      [(gc:cons? next)
       (if (eq? (heap-ref (+ next 4)) 'marked)
           (sweep-helper next (+ next (heap-ref (+ next 1))))
           (begin
             (heap-set! next 'free)
             (heap-set! (+ next 2) (unbox free-list))
             (set-box! free-list next)
             (sweep-helper next (+ next (heap-ref (+ next 1))))))])))

;(define (coalesce prev cur)
;  (begin
;    (define next (+ cur (heap-ref (+ cur 1))))
;    (when (< next (heap-size))
;      (when (eq? (heap-ref next) 'free)
;        (heap-set! (+ cur 1) (+ (heap-ref (+ cur 1)) (heap-ref (+ next 1))))
;        (heap-set! (
        
          
      