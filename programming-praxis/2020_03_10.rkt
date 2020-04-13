#lang racket
; https://programmingpraxis.com/2020/03/10/2sum/

; Using Racket's standard library
(define (2sum-standard lst target)
  (sequence-filter (λ (p) (= (apply + p) target))
                   (in-combinations lst 2)))

; Nested loops
(define (2sum-nested-loops lst target)
  (let ((len (length lst)))
    (foldl (λ (e1 i pairs)
             (foldl (λ (e2 pairs)
                      (if (= (+ e1 e2) target)
                          (cons (cons e1 e2) pairs)
                          pairs))
                    pairs (list-tail lst (+ i 1))))
           '() lst (range len))))


(define lst '(-1 4 3 0 1 2 5 8 1))

(display (sequence->list (2sum-standard lst 3)))
(newline)
(display (2sum-nested-loops lst 3))
