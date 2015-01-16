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
(prvih-n (tok-pascalovega-trikotnika) 5)

(define (stevilo-nacinov n)
  (define (zanka i)
    (cond [(> i n) 0]
          [(= i n) 1]
          [else (+ (stevilo-nacinov (- n i)) (zanka (+ i 1)))]))
  (if (< n 1)
    0
    (zanka 1)))
(displayln "\n=== Stevilo nacinov ===")
(stevilo-nacinov 5)

(define (stevilo-nacinov-memo n)
  (let ([memo (make-hash)])
    (lambda ()
      (define (zanka i)
        (cond [(> i n) 0]
              [(= i n) 1]
              [else (+ ((stevilo-nacinov-memo (- n i))) (zanka (+ i 1)))]))
      (hash-ref! memo n (if (< n 1) 0 (zanka 1))))))
(displayln "\n=== Stevilo nacinov memo ===")
((stevilo-nacinov-memo 1))
((stevilo-nacinov-memo 2))
((stevilo-nacinov-memo 3))
((stevilo-nacinov-memo 4))
((stevilo-nacinov-memo 5))


(define (vector-assoc v vec)
  (define (isci n)
    (let ([trenutni (vector-ref vec n)])
      (cond [(>= n (vector-length vec)) #f]
            [(pair? trenutni)
             (if (equal? (car trenutni) v)
               trenutni
               (isci (+ n 1)))])))
  (isci 0))

(define (cached-assoc sez n)
  (let ([cache (make-vector n #t)]
        [counter 0])
    (define (assocb x)
      (let ([odg (vector-assoc x cache)])
        (if odg
          odg
          (let ([novi-odg (assoc x sez)])
            ((vector-set! cache counter novi-odg)
             (set! counter (remainder (+ counter 1) n))
             novi-odg)))))
    assocb))

(define (odstej e1 e2) (sestej (e1 (negacija e2))))
(define (sestej-ali-odstej pogoj e1 e2)
  (ce-potem-sicer pogoj
                  (sestej e1 e2)
                  (odstej e1 e2)))
(define (nobeden e1 e2)
  (ce-potem-sicer e1
                  (bool false)
                  (ce-potem-sicer e2
                                  (bool false)
                                  (bool true))))
