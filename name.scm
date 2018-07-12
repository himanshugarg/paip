(define (titles)
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General))

(define (first-name name)
  (define (member? element ls)
    (cond ((null? ls) #f)
	  ((eq? element (car ls)) #t)
	  (else (member? element (cdr ls)))))
  (if (member? (car name) (titles))
      (first-name (cdr name))
      (car name)))

(first-name '(himanshu garg))
(first-name '(mr himanshu garg))

(define (last-name name)
  (define (reverse l)
    (define (helper l acc)
      (if (null? l)
	  acc
	  (helper (cdr l) (cons (car l) acc))))
    (helper l '()))
  (first-name (reverse name)))

(last-name '(himanshu garg jr))
