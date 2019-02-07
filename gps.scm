(define (every test? lst)
  (cond ((null? lst) #t)
	((test? (car lst)) (every test? (cdr lst)))
	(else #f)))

(define (member? item lst)
  (cond ((null? lst) #f)
	((eq? item (car lst)) #t)
	(else (member? item (cdr lst)))))

(define (some test? lst)
  (not (every (lambda (a) (not (test? a))) lst)))

(define (find-all test? arg lst)
  (cond ((null? lst) '())
	((test? arg (car lst)) (cons (car lst) (find-all test? arg (cdr lst))))
	(else (find-all test? arg (cdr lst)))))

(define (set-difference a b)
  (define (remove-element e lst)
    (cond ((null? lst) lst)
	  ((eq? e (car lst)) (cdr lst))
	  (else (cons (car lst) (remove-element e (cdr lst))))))
  (if (null? b)
      a
      (set-difference (remove-element (car b) a) (cdr b))))

(define (set-union a b)
  (cond ((null? a) b)
	((member (car a) b) (set-union (cdr a) b))
	(else (cons (car a) (set-union (cdr a) b)))))

(define (op-action op)
  (car op))

(define (op-preconds op)
  (cadr op))
  
(define (op-add-list op)
  (caddr op))

(define (op-delete-list op)
  (cadddr op))

(define (GPS state goals ops)
  (define (apply-operator op)
  ;; print a message and update *state* if op is applicable
    (if (every achieve (op-preconds op))
	(begin (display (list 'executing (op-action op)))
	       (newline)
	       (set! state (set-difference state (op-delete-list op)))
	       (set! state (set-union state (op-add-list op))))))
  (define (achieve goal)
  ;; a goal is achieved if it already holds
  ;; or if there is an appropriate op for it that is applicable
    (or (member? goal state)
	(some apply-operator
	      (find-all op-appropriate? goal ops))))
  ;; General Proplem Solver: achieve all goals using *ops*
  (if (every achieve goals) 'solved))

(define (op-appropriate? goal op)
  ;; an op is appropriate to a goal if it is in its add list
  (member? goal (op-add-list op)))


(define *school-ops*
	 '((drive-son-to-school
	    (son-at-home car-works) ; preconds
	    (son-at-school)         ; add-list	   
	    (son-at-home))          ; del-list
	   (shop-installs-battery
	    (car-needs-battery shop-knows-problem shop-has-money)
	    (car-works)
	    ())
	   (tell-shop-problem
	    (in-communication-with-shop)
	    (shop-knows-problem)
	    ())
	   (telephone-shop
	    (know-phone-number)
	    (in-communication-with-shop)
	    ())
	   (look-up-number
	    (have-phone-book)
	    (know-phone-number)
	    ())
	   (give-shop-money
	    (have-money)
	    (shop-has-money)
	    (have-money))))

(GPS '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)

(GPS '(son-at-home)
     '(son-at-school)
     *school-ops*)
