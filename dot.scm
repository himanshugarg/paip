(define (dot-product a b)
  (if (or (null? a) (null? b))
      '()
      (cons (* (car a) (car b)) (dot-product (cdr a) (cdr b)))))

(dot-product '(1 2) '(3 4))
