(define (*simple-grammar*)
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)))

(define (*bigger-grammar*)
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(define (*grammar*)
  (*bigger-grammar*))

(define (rule-lhs rule)
  (car rule))

(define (rule-rhs rule)
  (cdr (cdr rule)))

(define (non-terminal? category)
  (assoc category (*grammar*)))

(define (rewrites category)
  ;; return a list of possible rewrites for this category
  (rule-rhs (assoc category (*grammar*))))

(define (generate phrase)
  (define (list? item)
    (or (pair? item) (null? item)))
  (cond ((list? phrase)
	 (mappend generate phrase))
	((non-terminal? phrase)
	 (generate (random-elt (rewrites phrase))))
	(else (list phrase))))

(generate 'sentence)

;; Utility functions
(define (random-elt l)
  (define (length l)
    (if (null? l)
	0
	(+ 1 (length (cdr l)))))
  (define (element-at l i)
    (if (= i 0)
	(car l)
	(element-at (cdr l) (- i 1))))
  (element-at l (random (length l))))

(define (assoc key records)
  (cond ((null? records) #f)
	((eq? key (car (car records))) (car records))
	 (else (assoc key (cdr records)))))

(define (mappend f ls)
  (define (append l1 l2)
    (if (null? l1)
	l2
	(cons (car l1) (append (cdr l1) l2))))
  (if (null? ls)
      '()
      (append (f (car ls)) (mappend f (cdr ls)))))
