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

;Den poengløse prosedyren greet med defaultverdier for argumentene.
(define (greet . args)
  (let ((table (make-table)))
    
    ;Defaultverdier.
    (insert! 'time "day" table)
    (insert! 'title "friend" table)
    
    ;Lagrer håndtering av navngitte argumenter i en tabell.
    ;Kaller på den generelle hjelpeprosedyren.
    (set! table (help table args))
    
    ;Output.
    (display (string-append "good "
                            (lookup 'time table) " " (lookup 'title table)))
    (newline)))

;Generell hjelpeprosedyre som returnerer en tabell
;der (argumenter, verdier) som elementer.
(define (help table . args)
  (if (not (null? (car args)))
      (let ((arg (car args)))
        (if (not (null? (cdr arg)))
            (insert! (car arg) (cadr arg) table))
        (help table (cddr arg))))
  table)
          

;Test:
;(greet)
;(greet 'time "evening")
;(greet 'title "sir" 'time "morning")
;(greet 'time "afternoon" 'title "dear")



;============= OPPGAVE 2A ================


;Prosedyre for å konverte liste til strøm.
(define (list-to-stream list)
  (if (null? list)
      '()
      (cons-stream (car list) (list-to-stream (cdr list)))))

;Prosedyre for å konverte [n] elementer av strøm til liste.
(define (stream-to-list stream . n)
  (if (null? stream)
      '()
      (if (not (null? (cdr stream)))
          (if (null? n)
              (cons (stream-car stream) (stream-to-list (stream-cdr stream)))
              (if (> (car n) 1)
                  (cons (stream-car stream)
                        (stream-to-list (stream-cdr stream) (- (car n) 1)))
                  (cons (stream-car stream) '()))))))



;Test:
;(list-to-stream '(1 2 3 4 5))
;(define test-stream (list-to-stream '(8 3 5 7)))
;(stream-to-list test-stream)
;(stream-to-list (stream-interval 10 20))
;(stream-to-list nats 10)



;============= OPPGAVE 2B ================


(define (stream-map proc . argstreams)
  (if (null? argstreams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))


;Test:
;(define t1 (list-to-stream '(8 3 5 7)))
;(define t2 (list-to-stream '(6 7 4)))
;(define t3 (list-to-stream '(9 0 2 1 3 5)))
;(define test (stream-map + t1 t2 t3))
;(show-stream test 1)
;(show-stream test 2)
;(show-stream test 3)



;============= OPPGAVE 2C ================


;For det første, så vil memq ikke terminere dersom vi kjører med en uendelig
;lang strøm. For det andre, stream-cdr returnerer ikke en liste med et element
;som vanlig cdr ville ha gjort, men returnerer heller bare elementet. Vanlig cdr
;returnerer f.eks (cdr '(2 3)) -> (3. '()) eller (3). Men stream-cdr returnerer
;bare 3, bare elementet, når det ikke eksisterer flere promise i strømmen. Dette
;kan vi ikke ta stream-car på som memq vil ha oss til å gjøre.
;For det tredje så vil den returnere en stream på grunn av cons-stream, i stedet
;for en liste med alle unike variabler.



;============= OPPGAVE 2D ================



;Hjelpeprosedyre for å sjekke om et gitt element er lik
;eller ulik det første elementet i en strøm.
(define (check-first-stream-element stream)
  (lambda (given-element)
    (not (eq? given-element (stream-car stream)))))
        

;Filterer strømmen i rekursjonen med alle elementer som hjelpeprosedyren
;ovenfor returnerer som er ulike i strømmen.
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter
                     (check-first-stream-element stream) stream)))))


;Test:
;(define t4 (list-to-stream '(8 8 3 1 3 5 7 7)))
;(show-stream (remove-duplicates t4))



;============= OPPGAVE 2E ================

#|
REPL'et printer ut 0 1 2 3 4 5 5 6 7 7 nedover.
0 printes ut når x blir definert, dette er fordi stream-interval prosedyren
vil selv printe ut seg selv, så når (stream-map show (stream-interval 0 10))
blir kalt så vil stream-map sende show på første i intervallet, men siden
den er delayed så blir ikke resten kalt.

(stream-ref x 5) vil så printe ut 1 2 3 4 5 5, 1 2 3 4 5 blir printet ut først
fordi igjen, x er definert som å printe ut hele intervallet med show, så det
vil skje først, mens stream-ref bare vil ha den på posisjon 5, og derfor vil
5 printes ut etter. 0 printes ikke igjen siden det bare blir printet ut når
kalt første gangen som definert i x.

Derfor når (stream-ref x 7) blir kalt så printes ikke hele rekkefølgen til
intervallet på nytt, men delayen blir forced og printer derfor videre 6 og 7.
Og stream-ref vil da til slutt returnere 7.

Hvis stream-ref kommandoene hadde stått i en annen rekkefølge, f.eks 7 først og
så 5, så hadde den printet 1 2 3 4 5 6 7 7 og så 5 til slutt, fordi show på
intervallet i x allerede hadde blitt kalt, og stream-ref trengte bare å hente
verdien 5.
|#


;============= OPPGAVE 2F ================


(define (mul-streams m1 m2)
  (stream-map * m1 m2))


;Test:
;(define m1 (list-to-stream '(8 8 3 1)))
;(define m2 (list-to-stream '(2 2 2 2)))
;(define m3 (list-to-stream '(5 5 3 7)))
;(define m4 (list-to-stream '(0 1 2 3)))
;(mul-streams m1 m2)

;=====================TESTING FOR 2F==========
"2F"

;Test:

(define (mul-streams m1 m2)
  (stream-map * m1 m2))

(define (delist stream)
  (if (number? (car stream))
      stream
      (if (not (number? (car stream)))
          (delist (car stream))
          stream)))

(define (findsecond stream)
  (display "Heyo")
  (display stream)
  (if (not (null? (cdr stream)))
      (if (not (pair? (cdr stream)))
          (cdr stream)
          (findsecond (car stream)))))

(define (mul-streams2 stream . args)
  (display "start") (newline)
  (display "stream: ")
  (show stream)
  (display "args: ")
  (show args)
  (show (delist args))
  (let ((current (stream-map * stream (delist args))))
    (if (null? args)
        stream
        (if (not (null? (cdr args)))
            (mul-streams2 current (cdr args))
            (if (pair? (car args))
                (if (pair? (findsecond args))
                    stream
                    (mul-streams2 current (findsecond args))))))))


;Test:
(define m1 (list-to-stream '(8 8 3 1)))
(define m2 (list-to-stream '(2 2 2 2)))
(define m3 (list-to-stream '(5 5 3 7)))
(define m4 (list-to-stream '(0 1 2 3)))
(pair? (cdr m1))
"Test 1"
(show-stream (mul-streams2 m1 m2) 3)
"Test 2"
(define a (mul-streams2 m1 m2 m3 m4))
(show a)

"End of 2F"

;============= OPPGAVE 2G ================


(define factorials
  (cons-stream 1 (mul-streams factorials nats)))

;Test:
;(stream-ref factorials 5)
;(stream-ref factorials 0)
;(stream-ref factorials 1)