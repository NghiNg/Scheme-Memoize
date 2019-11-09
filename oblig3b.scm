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

(set! primitive-procedures (append primitive-procedures
                                   (cons (list '1+ (lambda (x) (+ x 1))) '())))
(set! primitive-procedures (append primitive-procedures
                                   (cons (list '1- (lambda (x) (- x 1))) '())))
(set! the-global-environment (setup-environment))


;the-global-environment
;2b


(define (install-primitive! title proc)
  (set! primitive-procedures (append primitive-procedures
                                   (cons (list title proc) '()))))

(set! primitive-procedures (append primitive-procedures
                                   (cons (list 'install-primitive!(lambda (title proc) (define-variable! title proc the-global-environment))) '())))

(set! the-global-environment (setup-environment))

#|
(define a '(1 2 3 4 5 6 7))
(set! a (reverse a))
(set! a (cons 8 a))
(reverse a)|#
#|

(define (and2 . args)
  (if (null? (cdr args))
      (car args)
      (if (eq? #f (car args))
          #f
          (apply and2 (cdr args)))))

;(mc-eval (and2 #t #t (= 1 2)) the-global-environment)


(define (or2 . args)
  (if (null? (cdr args))
      (car args)
      (if (eq? #t (car args))
          #t
          (apply or2 (cdr args)))))

|#
;(mc-eval (or2 #f #t #t #t) the-global-environment)

#|
(define (if2 test1 utfall1 utfall2 . args)
  (eq? #t test1)
  utfall1
  (if (null? args)
      utfall2
|#
      (read-eval-print-loop)
  