(load "evaluator.scm")

(set! the-global-environment (setup-environment))
(mc-eval '(+ 1 2) the-global-environment)

;Denne starter ui-en.
;(read-eval-print-loop)


;1a

#|
(define (foo cond else)
  (cond ((= cond 2) 0)
        (else (else cond))))
(define cond 3)
(define (else x) (/ x 2))
(define (square x) (* x x))
|#

;returnerer
;ok
;etter definisjonene
;(foo 2 square) gir 0
;(foo 4 square) gir 16

;(cond ((= cond 2) 0)
;      (else (else 4)))
;gir 2

;2a

(define (1+ arg) (+ arg 1))

(mc-eval (1+ 5) the-global-environment)
#|
|#

;2b

#|
(define (install-primitive! title proc)
  (extend-environment title proc nil))
(install-primitive! 'square (lambda (x) (* x x)))

(define a '(1 2 3 4 5 6 7))
(set! a (reverse a))
(set! a (cons 8 a))
(reverse a)|#


(define (and2 . args)
  (if (null? (cdr args))
      (car args)
      (if (eq? #f (car args))
          #f
          (apply and2 (cdr args)))))


(mc-eval (and2 #t #t (= 1 2)) the-global-environment)


(define (or2 . args)
  (if (null? (cdr args))
      (car args)
      (if (eq? #t (car args))
          #t
          (apply or2 (cdr args)))))


(mc-eval (or2 #f #t #t #t) the-global-environment)

#|
(define (if2 test1 utfall1 utfall2 . args)
  (eq? #t test1)
  utfall1
  (if (null? args)
      utfall2
|#
      
  