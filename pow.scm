(define (power a n)
  (if (= n 0)
      1
      (* a (power a (- n 1)))))

(power 2 3)

(power 4 10)
