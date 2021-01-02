#lang racket
;https://programmingpraxis.com/2020/03/27/list-rotation/

(define (rotations lst)
  (build-list (length lst)
              (lambda (n)
                (append (list-tail lst n) (take lst n)))))

