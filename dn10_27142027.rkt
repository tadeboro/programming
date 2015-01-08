#lang racket

; Tok števil
(define moj-generator-stevil
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 3) 0)
                        (- x)
                        x)
                      (lambda () f (+ x 1))))])
    (lambda () (f 1))))

; Izpiši prvih n elementov toka
(define (prvih-n gen n)
  (if (< n 1)
    (displayln (car (gen)))
    (let ([tok (gen)])
      (begin
        (displayln (car tok))
        (prvih-n (cdr tok) (- n 1))))))
;(prvih-n moj-generator-stevil 3)

; Generiraj poljuben tok
(define (generiraj-tok fun arg)
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (fun x arg)))))])
    (lambda () (f arg))))

; Definiraj naravna števila
(define naravna-st (generiraj-tok + 1))
(define potence-st-dva (generiraj-tok * 2))

; Vrni seznam začetka elementov, ki ne ustrezajo pogoju
(define (generiraj-dokler gen tester)
  (let ([tok (gen)])
    (if (tester (car tok))
      null
      (cons (car tok) (generiraj-dokler (cdr tok) tester)))))
;(define test (generiraj-dokler naravna-st (lambda (x) (x > 10))))

; Vrni element seznama
(define (seznam-po-modulu sez n)
  (cond [(null? sez) (error "Prazen seznam")]
        [(< n 0) (error "Negativen n")]
        [#t (car (list-tail sez (remainder n (length sez))))]))
(define (krozi-po-seznamih sez1 sez2)
  (letrec ([f (lambda (n)
                (cons (cons (seznam-po-modulu sez1 n)
                            (seznam-po-modulu sez2 n))
                      (lambda () f (+ n 1))))])
    (lambda () (f 0))))

; Makroji
;(define-syntax ime
;  (syntax-rules (kjucne besede)
;    [(vzorec) (izraz)]))

; Primer
; (izracunaj 3 + 2)
(define-syntax izracunaj
  (syntax-rules
    ()
    [(izracunaj arg1 op arg2) (op arg1 arg2)]))
(izracunaj 2 + 3)

; Makro za for zanko
(define-syntax for
  (syntax-rules
    (to do)
    [(for low to high do body)
     (let ([l low]
           [h high])
       (letrec ([loop (lambda (x)
                        (if (> x h)
                          #t
                          (begin body (loop (+ x 1)))))])
         (loop l)))]))
(for 2 to 5 do (displayln "a"))
