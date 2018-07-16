(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (sentence) (append (noun-phrase) (verb-phrase)))
(define (noun-phrase) (append (article) (noun)))
(define (verb-phrase) (append (verb) (noun-phrase)))
(define (article) (one-of '(the a)))
(define (noun) (one-of '(man ball woman table)))
(define (verb) (one-of '(hit took saw liked)))

(define (one-of l)
  (define (length l)
    (if (null? l)
	0
	(+ 1 (length (cdr l)))))
  (define (element-at l i)
    (if (= i 0)
	(car l)
	(element-at (cdr l) (- i 1))))
  (list (element-at l (random (length l)))))

(one-of '(2 3 4))
; Value: 2
    
(sentence)
