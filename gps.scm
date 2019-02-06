(define (*state*) '())

(define (*ops*) '())

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
  (cadr op)
  
(define (op-add-list op)
  (caddr op))

(define (op-delete-list op)
  (cadddr op))

(set-union '(1 2) '(1 2 3 4 5))
(set-difference '(1 2) '(3 4 5))
(every (lambda (a) (> a 1)) '(2 1 4))
(member? '5 '(1 2 3 4))
(some (lambda (a) (> a 1)) '(1 1 1))
(find-all (lambda (dummy a) (> a 4)) #t '(1 2 3 4))

(define (GPS goals *ops*)
  ;; General Proplem Solver: achieve all goals using *ops*
  (if (every achieve goals) 'solved))

(define (achieve goal)
  ;; a goal is achieved if it already holds
  ;; or if there is an appropriate op for it that is applicable
  (or (member? goal *state*)
      (some op-apply
	    (find-all *ops* op-appropriate? goal))))

(define (op-appropriate? goal op)
  ;; an op is appropriate to a goal if it is in its add list
  (member? goal (op-add-list op)))

(define (apply-operator op)
  ;; print a message and update *state* if op is applicable
  (when (every achieve (op-preconds op))
	(print (list 'executing (op-action op)))
	(setf! *state* (set-difference *state* (op-delete-list op)))
	(setf! *state* (set-union *state* (op-add-list op)))
	t))


