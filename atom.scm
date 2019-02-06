(define (count-atoms l)
  ; count the number of atoms in an expression.
  ; eg (count-atoms '(a (b) c)) = 3
  (cond ((null? l) 0)
	((pair? l) (+ (count-atoms (car l))
		      (count-atoms (cdr l))))
	 (else 1)))

(eq? 6 (count-atoms '(1 (2 3) 4 (4 5))))
(eq? 0 (count-atoms ()))


