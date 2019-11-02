(load "prekode3a.scm")(load "prekode3a.scm")


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

;1c)



(define (greet . args)
  (let ((table (make-table)))
    (insert! 'time "day" table)
    (insert! 'title "friend" table)
    (set! table (help table args))
      (display "good")
      (display " ")
      (display (lookup 'time table))
      (display " ")
      (display (lookup 'title table)) (newline)))

(define (help table . args)
  (if (not (null? (car args)))
      (let ((arg (car args)))
      (if (not (eq? '() (cdr arg)))
          (insert! (car arg) (cadr arg) table))
      (help table (cddr arg))))
  table)
          
      
(greet)
(greet 'time "evening")
(greet 'title "sir" 'time "morning")
(greet 'time "afternoon" 'title "dear")


;Oppgave 2a


(define (list-to-stream list)
  (cons-stream (car list) (cdr list)))


(list-to-stream '(1 2 3 4 5))


(define (stream-to-list stream . n)
  (if (null? n)
      ;(cons (stream-car stream) (stream-cdr stream))
      (if (not (null? stream))
          (cons (stream-car stream) (stream-to-list (stream-cdr stream)))
          '())
      (if (zero? (car n))
          '()
          (cons (stream-car stream)
                (stream-to-list (stream-cdr stream) (- (car n) 1))))))

(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

"HAR IKKE GJORT OPPGAVE 2B!!!"

;Oppgave 2c
#|
For det første så vil memq ikke terminere dersom vi kjører med en uendelig lang
strøm.
For det andre det stream-cdr returnerer ikke en liste med et element som vanlig
cdr ville ha gjort, vanlig cdr returnerer f.eks (cdr '(2 3)) -> (3. '()) eller
(3). Men stream-cdr returnerer bare 3, bare tallet. Og det kan vi ikke ta
stream-car på som memq vil ha oss til å gjøre. 



|#

