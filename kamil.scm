(define(c p n l)(if(= 0 n)l(cons(p)(c p(- n 1)l))))
(define(f c)(cond((char=? c #\T)2)((char=? c #\D)2)((char=? c #\L)2)((char=? c #\F)2)(else 1)))
(define(p s)(write(apply *(map f(string->list (symbol->string s)))))(newline))
(map p(c read 10 '()))
