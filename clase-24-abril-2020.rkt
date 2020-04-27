#lang racket
; Programa de Morse Clase 24 abril 2020
; Este porgrama es un menu en el cual estan las opciones de
; convertir texto de español a clave morse y convertir de clave morse al español
; Profesor: Francisco Alejandro Medina A
; Estudiante: Alejandro Vergara A

; Esta funcion recive una cadena de texto en español
; y devuelve un vector de caracteres en clave morse
(define (convMorse c n con)
  (if (< con (string-length c))
      (begin
        (vector-set! n con (dicMorse (string-ref c con)))
        (convMorse c n (+ con 1))
        )
      n
      )
  )


; Esta funcion recive un vector de caracteres en clave morse
; y devuelve una cadena de texto en español en mayuscula
(define (convEspanol v c con)
  (if (< con (vector-length v))
      (begin
        (string-set! c con (dicEspanol (vector-ref v con)))
        (convEspanol v c (+ con 1))
        )
      c
      )
  )

; Diccionario de morse
; esta funcion recive un character y devuelve
; su miso valor en clave morse
(define (dicMorse c)
  (cond
    [(or (equal? c #\A) (equal? c #\a)) "/.-"]
    [(or (equal? c #\B) (equal? c #\b)) "/-..."]
    [(or (equal? c #\C) (equal? c #\c)) "/-.-."]
    [(or (equal? c #\D) (equal? c #\d)) "/-.."]
    [(or (equal? c #\E) (equal? c #\e)) "/."]
    [(or (equal? c #\F) (equal? c #\f)) "/..-."]
    [(or (equal? c #\G) (equal? c #\g)) "/--."]
    [(or (equal? c #\H) (equal? c #\h)) "/...."]
    [(or (equal? c #\I) (equal? c #\i)) "/.."]
    [(or (equal? c #\J) (equal? c #\j)) "/.---"]
    [(or (equal? c #\K) (equal? c #\k)) "/-.-"]
    [(or (equal? c #\L) (equal? c #\l)) "/.-.."]
    [(or (equal? c #\M) (equal? c #\m)) "/--"]
    [(or (equal? c #\N) (equal? c #\n)) "/-."]
    [(or (equal? c #\O) (equal? c #\o)) "/---"]
    [(or (equal? c #\P) (equal? c #\p)) "/.--."]
    [(or (equal? c #\Q) (equal? c #\q)) "/--.-"]
    [(or (equal? c #\R) (equal? c #\r)) "/.-."]
    [(or (equal? c #\S) (equal? c #\s)) "/..."]
    [(or (equal? c #\T) (equal? c #\t)) "/-"]
    [(or (equal? c #\U) (equal? c #\u)) "/..-"]
    [(or (equal? c #\V) (equal? c #\v)) "/...-"]
    [(or (equal? c #\W) (equal? c #\w)) "/.--"]
    [(or (equal? c #\X) (equal? c #\x)) "/-..-"]
    [(or (equal? c #\Y) (equal? c #\y)) "/-.--"]
    [(or (equal? c #\Z) (equal? c #\z)) "/--.."]
    [(equal? c #\space ) "/"]
    [(equal? c #\0) "/-----"]
    [(equal? c #\1) "/.----"]
    [(equal? c #\2) "/..---"]
    [(equal? c #\3) "/...--"]
    [(equal? c #\4) "/....-"]
    [(equal? c #\5) "/....."]
    [(equal? c #\6) "/-...."]
    [(equal? c #\7) "/--..."]
    [(equal? c #\8) "/---.."]
    [(equal? c #\9) "/----."])
  )


; Diccionario de español
; esta funcion recive un valor en clave morse y devuelve
; el character al que hace referencia ese valor
(define (dicEspanol c)
  (cond
    [(string=? c ".-") #\A]
    [(string=? c "-...") #\B]
    [(string=? c "-.-.") #\C]
    [(string=? c "-..") #\D]
    [(string=? c ".") #\E]
    [(string=? c "..-.") #\F]
    [(string=? c "--.") #\G]
    [(string=? c "....") #\H]
    [(string=? c "..") #\I]
    [(string=? c ".---") #\J]
    [(string=? c "-.-") #\K]
    [(string=? c ".-..") #\L]
    [(string=? c "--") #\M]
    [(string=? c "-.") #\N]
    [(string=? c "---") #\O]
    [(string=? c ".--.") #\P]
    [(string=? c "--.-") #\Q]
    [(string=? c ".-.") #\R]
    [(string=? c "...") #\S]
    [(string=? c "-") #\T]
    [(string=? c "..-") #\U]
    [(string=? c "...-") #\V]
    [(string=? c ".--") #\W]
    [(string=? c "-..-") #\X]
    [(string=? c "-.--") #\Y]
    [(string=? c "--..") #\Z]
    [(string=? c (make-string 0)) #\space]
    [(string=? c "-----") #\0]
    [(string=? c ".----") #\1]
    [(string=? c "..---") #\2]
    [(string=? c "...--") #\3]
    [(string=? c "....-") #\4]
    [(string=? c ".....") #\5]
    [(string=? c "-....") #\6]
    [(string=? c "--...") #\7]
    [(string=? c "---..") #\8]
    [(string=? c "----.") #\9]
    (#\@))
  )

;Esta funcion imprime los elementos de un vector por consola
(define (mostrarVector v c)
  (if (< c (vector-length v))
      (begin
        (display (vector-ref v c))
        (mostrarVector v (+ c 1))
        )
      (void)
      )
  
  )

;esta funcion muestra un menu para convertir de texto a morse y viceversa
(define (menu)
  (display "\n\r\tBienvenido al menu de Morse\n")
  (display "\ten este menu podra convertir texto español\n\t a simbolos en codigo morse y viceversa\n\r")
  (display "\tAcontinuacion observe el siguiente menu y elija una opcion.\n")
  (display "\tMenu de Morse\n")
  (display "\t[1-]. Convertir texto en español a codigo Morse.\n")
  (display "\t[2-]. Convertir simbolos de codigo morse a texto en español.\n")
  (display "\t[3-]. Cerrar el programa.\n")
  (define opcion (read))
  (cond
    [(= opcion 1) (morse)]
    [(= opcion 2) (begin
                    (display "\n\tNOTA: ejemplo /.-/-.../-.-.")
                    (display "\n\tIngresar el codigo -->")
                    (read-line)
                    (define n (read-line))
                    (define c (contarCharacters n 0 0))
                    (define c2 (convEspanol (llenarVectorMorse n (make-vector c ) 0 0 (make-string 0)) (make-string c) 0))
                    (display "\n\tClave Morse en Español :")
                    (display (string-append "\r\n\t" c2))
                    )]
    )
    (if (= opcion 3)
        (display "Se cerro el programa")
        (menu))
  )

; Esta funcion cuenta los caracteres de un codigo morse
(define (contarCharacters n c c1)
  (if (> (string-length n) c)
      (if (equal? (string-ref n c) #\/)
          (contarCharacters n (+ c 1) (+ c1 1))
          (contarCharacters n (+ c 1) c1)
          )
      c1
      )
  )

;esta funcion ejecuta la conversion a clave morse
(define (morse)
  (display "\n\tIngresar el texto->")
  (read-line)
  (define c (read-line))
  (define v (convMorse c (make-vector (string-length c) ) 0))
  (display "\n\tTexto en clave Morse :")
  (display "\n\r\t")
  (mostrarVector v 0))

; Esta funcion recive una cadena en clave morse  y un vector vacio
; devuelve un vector con los caracteres en morse de la cadena
(define (llenarVectorMorse n v cc cv c)
  (if (> (string-length n) cc)
      (begin
        (if (and (equal? (string-ref n cc) #\/) (equal? (string-ref n (+ cc 1)) #\/))
            (begin
              (vector-set! v cv (string #\/ #\/))
              (llenarVectorMorse n v (+ cc 2) (+ cv 1) (make-string 0)))
            (void))
        (if (equal? (string-ref n cc) #\/)
            (if (= cc 0)
                (begin
                  (llenarVectorMorse n v (+ cc 1) cv c ))
                (begin
                  (vector-set! v cv c)
                  (llenarVectorMorse n v (+ cc 1) (+ cv 1) (make-string 0))
                  )
              )
            (begin
              (if (= (string-length n) (+ cc 1))
                  (vector-set! v cv (string-append c (substring n cc (+ cc 1))))
                  (void))
              (llenarVectorMorse n v (+ cc 1) cv (string-append c (substring n cc (+ cc 1)))))
            )
        )
      v
      )
  )


; LLamado del menu
; ejecucion...
(menu)
