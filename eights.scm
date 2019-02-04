(define (call-n-times proc n l)
        (if (= 0 n)
            l
            (call-n-times proc (- n 1) (cons (proc) l))))

(define (kth-number-whose-cube-ends-in-888 k)
        (+ (* 1000 (quotient (- k 1) 4)) (cond ((= (remainder (- k 1) 4) 0) 192)
                                               ((= (remainder (- k 1) 4) 1) 442)
                                               ((= (remainder (- k 1) 4) 2) 692)
                                               ((= (remainder (- k 1) 4) 3) 942))))

(map (lambda (x) (write x) (newline))
     (map kth-number-whose-cube-ends-in-888 (reverse (call-n-times read (read) '()))))
