; for lists of length > 2 local-inversions are the same as the number
; of local-inversions for the remaining list + 0 or 1 depending on 
; whether the first element is less than the second or not
(define (local-inversions l)
        (cond ((< (length l) 2) 0)
              ((> (car l) (cadr l)) (+ 1 (local-inversions (cdr l))))
              (else                 (+ 0 (local-inversions (cdr l))))))

; for lists of length > 2 inversions are the same as the number of elements
; against which the first is greater + the inversions of the remaining
(define (inversions l)
        (cond ((< (length l) 2) 0)
              (else (+ (length (filter (lambda (x) (> (car l) x)) (cdr l)))
                       (inversions (cdr l))))))

(define (enumerate-interval low high)
        (if (> low high)
            '()
            (cons low (enumerate-interval (+ low 1) high))))

(use-modules (ice-9 rdelim))

(define (call-n-times proc n)
        (if (= 0 n)
            '()
            (cons proc (call-n-times proc (- n 1)))))

(define (solve t)
        (let* ((count  (read))
               (l      (call-n-times read count)
        (if (= (local-inversions l) (inversions l))
            (write-line "YES")
            (write-line "NO" ))))

(for-each solve (enumerate-interval 1 (string->number (read-line))))
