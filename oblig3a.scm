(load "prekode3a.scm")


;============= OPPGAVE 1A og 1B ================

;Prosedyre som både memoiserer og avmemoiserer prosedyren proc.
(define (mem message proc)

  ;Lagring av den originale prosedyren.
  (let ((orig-proc proc))        
    (set! orig-proc proc)

    ;Memoisering av prosedyren.
    (cond ((eq? message 'memoize)
           (let ((table (make-table)))       
             (lambda args
               (if (not (eq? #f (lookup args table)))
                   (lookup args table)
                   (let ((result (apply proc args)))            
                     (insert! args result table)
                     result)))))

          ;Avmemoiseringen av prosedyren.
          ((eq? message 'unmemoize) orig-proc)

          ;Alle andre tilfeller.
          (else 'Error))))

;Test:
;(set! fib (mem 'memoize fib))
;(fib 3)
;(fib 3)
;(fib 2)
;(fib 4)
;(set! fib (mem 'unmemoize fib))
;(fib 3)
;(set! test-proc (mem 'memoize test-proc))
;(test-proc)
;(test-proc)
;(test-proc 40 41 42 43 44)
;(test-proc 40 41 42 43 44)
;(test-proc 42 43 44)
;(set! test-proc (mem 'unmemoize test-proc))



;============= OPPGAVE 1C ================

;(define mem-fib (mem 'memoize fib))
;(mem-fib 3)
;(mem-fib 3)
;(mem-fib 2)


;Den memoiserte prosedyren til fib blir aliaset som
;mem-fib. Problemet er at mem-fib kun memoiserer
;verdiene til parameterne den blir kalt på med, og ikke
;alle andre verdier opp t.o.m. denne verdien. Omgivelsen
;dens blir utvidet med en ny ramme for hver kall og retur-
;verdien blir lagret i memoiseringstabellen. I oppgave 1a
;så memoiserte vi destruktivt slik at hvert kall har den
;memoiserte versjonen som paramter. Da vil alle verdier
;som blir beregnet av fib lagret som i samme ramme. 



;============= OPPGAVE 1D ================


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
For det tredje så vil den returnere en stream på grunn av cons-stream, i stedet
for en liste med alle unike variabler.

|#

;Oppgave 2d
(define (memq2 item x)
  (cond ((number? x) #f)
        ((eq? item (stream-car x)) x)
        (else (memq2 item (stream-cdr x)))))

(define (remove-duplicates lst)
  (cond ((number? lst) (cons lst '()))
        ((not (memq2 (stream-car lst) (stream-cdr lst)))
         (cons (stream-car lst) (remove-duplicates (stream-cdr lst))))
        (else (remove-duplicates (stream-cdr lst)))))
