(define (titles)
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General Jr))

(define (member? element ls)
  (cond ((null? ls) #f)
	((eq? element (car ls)) #t)
	(else (member? element (cdr ls)))))

(define (first-name name)
  (if (member? (car name) (titles))
      (first-name (cdr name))
      (car name)))

(first-name '(himanshu garg))
(first-name '(mr himanshu garg))

(define (reverse l)
  (define (add-at-end l acc)
    (if (null? l)
	acc
	(add-at-end (cdr l) (cons (car l) acc))))
  (add-at-end l '()))

(define (last-name name)
  (first-name (reverse name)))

(eq? 'garg (last-name '(himanshu garg jr)))

(eq? 'Garg (first-name (reverse '(Himanshu Garg Jr))))
