#lang racket

; Izpiši prvih n elementov toka
(define (prvih-n gen n)
  (if (< n 2)
    (displayln (car (gen)))
    (let ([tok (gen)])
      (begin
        (displayln (car tok))
        (prvih-n (cdr tok) (- n 1))))))

; Tok paskalovega trikotnika
(define (tok-pascalovega-trikotnika)
  (define (gen-naslednjo trenutna)
    (define (ustvari-naslednjo trenutna)
      (cond [(null? trenutna) null]
            [(null? (cdr trenutna)) '(1)]
            [else (cons (+ (car trenutna) (cadr trenutna))
                        (ustvari-naslednjo (cdr trenutna)))]))
    (cons 1 (ustvari-naslednjo trenutna)))
  (define (prideluj-trikotnik trenutna)
    (let ([naslednja (gen-naslednjo trenutna)])
      (cons naslednja (lambda () (prideluj-trikotnik naslednja)))))
  (lambda () (prideluj-trikotnik null)))
(define tok (tok-pascalovega-trikotnika))

(displayln "\n=== Tok pascalovih stevil ===")
(prvih-n (tok-pascalovega-trikotnika) 3)
