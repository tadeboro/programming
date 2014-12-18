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
      (cons (f (car sez)) (moj-map (cdr sez)))))
;(moj-map (lambda (x) (* x 2)) '(1 2 3))

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

; Ućinkovita vrsta
(define (queue)
  (list null null)
(define (empty-queue? q)
  (and (null? (car q)) (null? (car (cdr q)))))
(define (enqueue el q)
  (cons (cons el (car q)) (cdr q)))
(define (dequeue q)
  (cons [(empty-queue? q) (cons void q)]
        [(null? (car (cdr q))) (dequeue (cons null (reverse (car q))))]
        [#t (car (car (cdr q))) (cons (car q) (cdr (car (cdr q))))]))

(queue)
(define (q (enqueue 2 (enqueue 1 (queue)))))
(dequeue q)