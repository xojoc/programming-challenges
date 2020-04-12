#lang racket
; https://programmingpraxis.com/2020/04/10/nth-item-in-a-linked-list/

(define/contract (my-list-ref lst idx)
  (pair? natural? . -> . any)
  (if (= idx 0)
      (car lst)
      (my-list-ref (cdr lst) (- idx 1))))
