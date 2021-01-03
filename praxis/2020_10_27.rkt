#lang racket
; https://programmingpraxis.com/2020/10/27/list-slices/

(define (split-at-or-null list length)
  (let l ([prefix '()]
          [suffix list]
          [length length])
    (if (<= length 0)
        (values (reverse prefix) suffix)
        (if (empty? suffix)
            (l (cons null prefix) suffix (- length 1))
            (l (cons (car suffix) prefix) (cdr suffix) (- length 1))))))

(define (sublists list lengths)
  (let l ([sublists '()]
          [list list]
          [lengths lengths])

    (if (empty? lengths)
        (reverse sublists)
        (let-values ([(prefix suffix) (split-at-or-null list (car lengths))])
          (l (cons prefix sublists)
             suffix
             (cdr lengths))))))
            
(split-at-or-null '(1 2 3) 1)
(split-at-or-null '(1 2 3) 4)
(sublists '(1 2 3 4 5 6) '(1 2 4))
(sublists '(1 2 3 4 5 6) '(1 2))
