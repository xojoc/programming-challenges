#lang racket
; https://programmingpraxis.com/2020/10/06/pandigital-squares-faster-and-smaller/

(define (digits->number digits)
  (foldl (λ (d n) (+ (* n 10) d)) 0 digits))

(define (square? n)
  (let ([isq (integer-sqrt n)])
    (= (* isq isq) n)))

(define (pandigitals)
  (sequence-filter (λ (lst) (not (= (car lst) 0)))
                   (in-permutations '(0 1 2 3 4 5 6 7 8 9))))

(define (pandigital-squares)
  (sequence-filter square?
                   (sequence-map digits->number (pandigitals))))

(sequence-length (pandigital-squares))