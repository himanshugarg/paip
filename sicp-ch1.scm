					; 1.1
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))


(+ 2 (if ( > b a) b a))

(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))


					; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 7 2)))

					; 1.3
(define (sum-larger-two a b c)
  (cond ((and (<= a b) (<= a c)) (+ b c))
	((and (<= b a) (<= b c)) (+ a c))
	((and (<= c a) (<= c b)) (+ a b))))
(= 1 (sum-larger-two 1 0 0))

					; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(= 1 (a-plus-abs-b 0 -1))

					; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
