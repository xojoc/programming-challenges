#lang racket

(require (only-in srfi/13 string-reverse))

(define (backwards-compare a b)
  (string<? (string-reverse a)
            (string-reverse b)))

(define (rhyming-sort lst)
  (sort lst backwards-compare))

(rhyming-sort '("falsely" "fly" "freely" "sorely" "surely"))