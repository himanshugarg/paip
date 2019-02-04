(define (count-bigger-than x l count)
        (cond ((null? l) count)
              ((> x (car l)) (count-bigger-than x (cdr l) (+ count 1)))
              (else          (count-bigger-than x (cdr l) count))))

; for lists of length > 2 inversions are the same as the number of elements
; against which the first is greater + the inversions of the remaining
(define (inversions l count)
        (if (< (length l) 2) 
            count
            (inversions (cdr l) (+ count 
                                   (count-bigger-than (car l) (cdr l) 0)))))

(define (call-n-times proc n l)
        (if (= 0 n)
            l
            (call-n-times proc (- n 1) (cons (proc) l))))

(define (solve)
        (write (inversions (reverse (call-n-times read (read) '())) 0))
        (newline))

;(call-n-times solve (read) '())

(write (inversions (call-n-times (lambda () (random 10000000)) 200000 '()) 0))
