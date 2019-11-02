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




;============= OPPGAVE 2C ================


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


;============= OPPGAVE 2D ================

(define (memq2 item x)
  (cond ((number? x) #f)
        ((eq? item (stream-car x)) x)
        (else (memq2 item (stream-cdr x)))))

(define (remove-duplicates lst)
  (cond ((number? lst) (cons lst '()))
        ((not (memq2 (stream-car lst) (stream-cdr lst)))
         (cons (stream-car lst) (remove-duplicates (stream-cdr lst))))
        (else (remove-duplicates (stream-cdr lst)))))
