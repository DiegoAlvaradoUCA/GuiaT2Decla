#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1 – Suma de elementos de una lista  => 15
(define (ej1-suma-lista xs)
  (foldl + 0 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2 – Filtrar mayores a n  => '(5 8 10) con n=4
(define (ej2-filtrar-mayores xs n)
  (filter (lambda (x) (> x n)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3 – Producto de una lista  => 24
(define (ej3-producto xs)
  (foldl * 1 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4 – Aplicar una función a todos los elementos (map)
(define (ej4-aplicar f xs)
  (map f xs))
(define (duplicar x) (* 2 x)) ; función de apoyo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5 – Cuadrado de números  => '(1 4 9 16 25)
(define (ej5-cuadrados xs)
  (map (lambda (x) (* x x)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 6 – Duplicar los valores de una lista  => '(4 8 12)
(define (ej6-duplicar-lista xs)
  (map (lambda (x) (* 2 x)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 7 – Seleccionar los números pares  => '(2 4 6)
(define (ej7-pares xs)
  (filter even? xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 8 – Promedio de una lista  => 7.0
;; (La guía muestra 7.0 como número inexacto)
(define (ej8-promedio xs)
  (cond [(null? xs) (error 'ej8-promedio "lista vacía")]
        [else (/ (exact->inexact (foldl + 0 xs))
                 (length xs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 9 – Elevar los elementos a una potencia n  => '(4 9 16) con n=2
(define (ej9-potencias xs n)
  (map (lambda (x) (expt x n)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 10 – Contar elementos > n  => 3 con n=6
(define (ej10-contar-mayores xs n)
  (length (filter (lambda (x) (> x n)) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 11 – Aplicar una función dos veces (sqrt 16) => 2.0
(define (ej11-aplicar-dos-veces f x)
  (f (f x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 12 – Mini reto: producto de los > 5  => 480
(define (ej12-producto-mayores-que xs n)
  (foldl * 1 (filter (lambda (x) (> x n)) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERIFICADOR: imprime EXACTAMENTE los casos de la guía
(module+ main
  (displayln "=== GUIA TALLER 2 ===")

  ;; Ej.1
  (define e1-input '(1 2 3 4 5))
  (displayln (format "E1: ~a => ~a "
                     e1-input (ej1-suma-lista e1-input)))

  ;; Ej.2
  (define e2-input '(5 3 8 1 10))
  (displayln (format "E2: mayores a 4 en ~a => ~a"
                     e2-input (ej2-filtrar-mayores e2-input 4)))

  ;; Ej.3
  (define e3-input '(2 3 4))
  (displayln (format "E3: producto ~a => ~a"
                     e3-input (ej3-producto e3-input)))

  ;; Ej.4
  (define e4-input '(1 2 3 4))
  (displayln (format "E4: aplicar duplicar a ~a => ~a"
                     e4-input (ej4-aplicar duplicar e4-input)))

  ;; Ej.5
  (define e5-input '(1 2 3 4 5))
  (displayln (format "E5: cuadrados de ~a => ~a"
                     e5-input (ej5-cuadrados e5-input)))

  ;; Ej.6
  (define e6-input '(2 4 6))
  (displayln (format "E6: duplicar ~a => ~a"
                     e6-input (ej6-duplicar-lista e6-input)))

  ;; Ej.7
  (define e7-input '(1 2 3 4 5 6))
  (displayln (format "E7: pares de ~a => ~a"
                     e7-input (ej7-pares e7-input)))

  ;; Ej.8
  (define e8-input '(4 6 8 10))
  (displayln (format "E8: promedio de ~a => ~a"
                     e8-input (ej8-promedio e8-input)))

  ;; Ej.9
  (define e9-input '(2 3 4))
  (displayln (format "E9: potencias ^2 de ~a => ~a"
                     e9-input (ej9-potencias e9-input 2)))

  ;; Ej.10
  (define e10-input '(5 7 2 9 1 10))
  (displayln (format "E10: contar >6 en ~a => ~a"
                     e10-input (ej10-contar-mayores e10-input 6)))

  ;; Ej.11
  (displayln (format "E11: aplicar dos veces sqrt a 16 => ~a"
                     (ej11-aplicar-dos-veces sqrt 16)))

  ;; Ej.12
  (define e12-input '(2 5 6 8 3 10))
  (displayln (format "E12: producto de >5 en ~a => ~a"
                     e12-input (ej12-producto-mayores-que e12-input 5))))
