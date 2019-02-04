(define (call-n-times proc n l)
        (if (= 0 n)
            l
            (call-n-times proc (- n 1) (cons (proc) l))))

(write (apply + (filter (lambda (x) (> x 0)) (call-n-times read (read) '()))))
(newline)
