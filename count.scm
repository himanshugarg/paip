(define (count-expr e1 e2)
  ; count the number of times an expression appears anywhere within
  ; another eg. (count-anywhere 'a '(a ((a) b) a))) = 3  
  (cond ((null? e2) 0)
	((eq? e1 e2) 1)
	((pair? e2) (+ (count-expr e1 (car e2))
		       (count-expr e1 (cdr e2))))
	(else 0)))

(count-expr 'a '(a (a ((a) b) a)))
