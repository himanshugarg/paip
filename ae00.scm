(define (rectangles-of-breadth b l max-length count)
        (if (> (* b l) max-length)
            count
            (rectangles-of-breadth b (+ l 1) max-length (+ count 1))))

(define (enumerate-interval low high l)
        (if (> low high)
            l
            (enumerate-interval (+ low 1) high (cons low l))))

; count rectangles of a given breadth starting from 1 and going upto n
(define (rectangles n)
        (write (apply + (map (lambda (x) (rectangles-of-breadth x x n 0))
                             (enumerate-interval 1 n '())))))

(rectangles (read))
