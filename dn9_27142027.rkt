#lang racket

; Osnovne definicije
(define s "hello")
(define x 3)
(define y (+ x 2))

(define kub1
  (lambda (x)
    (* x (* x x))))

(define (kub2 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))
; klic
((pow2 2) 3)


; Vsota seznama
(define (vsota list)
  (if (null? list)
      0
      (+ (car list) (vsota (cdr list)))))
(vsota '(1 2 3 4))

; moj-append
(define (moj-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (moj-append (cdr xs) ys))))
(moj-append '(1 2 3 4 5) '(6 7 8))

; moj-map
(define (moj-map f sez)
  (if (null? sez)
      null
      (cons (f (car sez)) (moj-map f (cdr sez)))))
(moj-map (lambda (x) (* x 2)) '(1 2 3))

; moj-reverse
(define (moj-reverse xs)
  (define (reverse-acc xs acc)
    (if (null? xs)
        acc
        (reverse-acc (cdr xs) (cons (car xs) acc))))
  (reverse-acc xs null))
(moj-reverse '(1 2 5 6))

; odstrani ponovljene elemente seznama
(define (odstrani-ponovljene sez)
  (cond [(null? sez) null]
        [(null? (cdr sez)) sez]
        [#t (let ([prvi (car sez)]
                  [drugi (car (cdr sez))])
              (if (equal? prvi drugi)
                  (odstrani-ponovljene (cdr sez))
                  (cons prvi (odstrani-ponovljene (cdr sez)))))]))
(odstrani-ponovljene '(1 1 1 2 2 3 4 4))

; Učinkovita vrsta
(define (queue)
  (list null null))
(define (empty-queue? q)
  (and (null? (car q)) (null? (cadr q))))
(define (enqueue el q)
  (cons (cons el (car q)) (cdr q)))
(define (dequeue q)
  (cond [(empty-queue? q) (cons void q)]
        [(null? (cadr q)) (dequeue (list null (reverse (car q))))]
        [#t (cons (caadr q) (list (car q) (cdadr q)))]))
(queue)
(enqueue 2 (enqueue 1 (queue)))
(empty-queue? (queue))
(empty-queue? (list '(1 2) null))
(dequeue (queue))
(dequeue (list '(1 2) '(3 4 5)))
(dequeue (list '(1 2) null))

; Paskalov trikotnik
(define (pascalov-trikotnik n)
  (define (pasc-level level)
    (
  (define (pasc-acc n l acc)
    (if [> n l]
      (reverse acc)
      (pasc-acc n (+ l 1) (cons level acc))))
  (pasc-acc n 1 null))