#lang racket

; Tok števil
(define moj-generator-stevil
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 3) 0) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
(displayln "\n=== moj-generator-stevil ===")
(car (moj-generator-stevil))
(car ((cdr (moj-generator-stevil))))
(car ((cdr ((cdr (moj-generator-stevil))))))

; Izpiši prvih n elementov toka
(define (prvih-n gen n)
  (if (< n 2)
    (displayln (car (gen)))
    (let ([tok (gen)])
      (begin
        (displayln (car tok))
        (prvih-n (cdr tok) (- n 1))))))
(displayln "\n=== prvih-n moj-generator-stevil 6 ===")
(prvih-n moj-generator-stevil 6)

; Generiraj poljuben tok
(define (generiraj-tok fun arg)
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (fun x arg)))))])
    (lambda () (f arg))))
; Definiraj naravna števila in potence števila 2
(displayln "\n=== generiraj-tok + 1 ===")
(define naravna-st (generiraj-tok + 1))
(prvih-n naravna-st 6)
(displayln "\n=== generiraj-tok * 2 ===")
(define potence-st-dva (generiraj-tok * 2))
(prvih-n potence-st-dva 6)

; Vrni seznam zaporednih elementov toka. Elemente dodajaj v seznam
; dokler tester vrača #f.
(define (generiraj-dokler gen tester)
  (define (gen-acc gen acc)
    (let ([tok (gen)])
      (if (tester (car tok))
        (reverse acc)
        (gen-acc (cdr tok) (cons (car tok) acc)))))
  (gen-acc gen null))
(displayln "\n=== generiraj-dokler naravna-st (> x 10) ===")
(generiraj-dokler naravna-st (lambda (x) (> x 10)))

; Vrni element seznama
(define (seznam-po-modulu sez n)
  (cond [(null? sez) (error "Prazen seznam")]
        [(< n 0) (error "Negativen n")]
        [#t (car (list-tail sez (remainder n (length sez))))]))
(define (krozi-po-seznamih sez1 sez2)
  (letrec ([f (lambda (n)
                (cons (cons (seznam-po-modulu sez1 n)
                            (seznam-po-modulu sez2 n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))
(displayln "\n=== krozi-po-seznamih '(1 2 3) '(4 5) ===")
(prvih-n (krozi-po-seznamih '(1 2 3) '(4 5)) 6)

; Funkcija, ki pove, če je število n deljivo z vsaj enim izmed števil v
; seznamu sez.
(define (deli sez n)
  (cond [(null? sez) #f]
        [(= (remainder n (car sez)) 0) #t]
        [else (deli (cdr sez) n)]))
; Funkcija, ki vrne naslednje prastevilo glede na seznam. Praštevila v
; seznamu morajo biti urejena v padajočem vrstnem redu.
(define (naslednje-prastevilo sez)
  (if (null? sez)
    2
    (let ([rsez (reverse sez)])
      (define (np n)
        (if (deli rsez n) (np (+ n 1)) n))
      (np (car sez)))))
; Tok praštevil
(define tok-prastevil
  (let ([prastevila null])
    (letrec ([f (lambda (x)
                  (let ([n (naslednje-prastevilo prastevila)])
                    (begin
                      (set! prastevila (cons n prastevila))
                      (cons n (lambda () (f n))))))])
      (lambda () (f 2)))))
(displayln "\n=== tok-prastevil ===")
(prvih-n tok-prastevil 10)

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
(displayln "\n=== izracunaj 2 + 3 ===")
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
(displayln "\n=== for 2 to 5 do (display \"a\" ===")
(for 2 to 5 do (display "a"))

; Število elementov v toku, ki jih srečamo pred izpolnitvijo pogoja.
(define (stevilo-preden gen pogoj)
  (define (st-acc gen n)
    (let ([tok (gen)])
      (if (pogoj (car tok))
        n
        (st-acc (cdr tok) (+ n 1)))))
  (st-acc gen 0))
(displayln "\n===  stevilo-preden naravna > 10 ===")
(stevilo-preden naravna-st (lambda (x) (> x 10)))

; While less makro
(define-syntax while-less
  (syntax-rules
    (do)
    [(while-less e1 do e2)
     (let ([e1val e1])
       (letrec ([loop (lambda ()
                        (let ([e2val e2])
                          (if (> e1val e2val) (loop) #t)))])
         (loop)))]))
(displayln "\n===  while-less ===")
(define a 6)
(while-less 5 do (begin (set! a (+ a 1)) (display "x ") a))
(set! a 1)
(while-less 5 do (begin (set! a (+ a 1)) (display "x ") a))
