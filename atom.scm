(define (count-atoms l)
  (cond ((null? l) 0)
	((pair? l) (+ (count-atoms (car l))
		      (count-atoms (cdr l))))
	 (else 1)))

(count-atoms '(1 (2 3) 4 (4 5)))


