(load "prekode3a.scm")


;============= OPPGAVE 1A ================
#|
(define (mem message proc)
  (let ((table (make-table)))
    (lambda args
      (if (not (eq? #f (lookup args table)))
          (lookup args table)
          (let ((result (apply proc args)))            
            (insert! args result table)
            result)))))
|#





;============= OPPGAVE 1B ================


;(define (orig-proc proc)
 ; (set! orig-proc proc))

(define (mem message proc)
    
    (let ((orig-proc proc))
             (set! orig-proc proc)

    (cond ((eq? message 'memoize)
           (let ((table (make-table)))       
             (lambda args
               (if (not (eq? #f (lookup args table)))
                   (lookup args table)
                   (let ((result (apply proc args)))            
                     (insert! args result table)
                     result)))))

          ((eq? message 'unmemoize) orig-proc)
          
          (else 'Error))))
#|
(define (mem message proc)
    
    (let ((orig-proc proc))
             (set! orig-proc proc)

    (cond ((eq? message 'memoize)
           (let ((table (make-table)))       
             (lambda args
               (if (not (eq? #f (lookup args table)))
                   (lookup args table)           
                   (insert! args (apply proc args) table))
               (apply proc args))))

          ((eq? message 'unmemoize) orig-proc)
          
          (else 'Error))))
|#


;Test:
"AAAAHHHHH"
#|
(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)

(set! fib (mem 'unmemoize fib))
(fib 3)


(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)
(set! test-proc (mem 'unmemoize test-proc))
|#
#|
"BAAAAAHHH"
(define mem-fib (mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
|#