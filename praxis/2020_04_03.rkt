#lang racket
; https://programmingpraxis.com/2020/04/03/homework-3/

(define/contract (addends C N)
  (positive? positive? . -> . (non-empty-listof natural?))
  (let* ((avg (floor (/ C N)))
         (above (- C (* N avg))))
    (append (make-list above (+ avg 1))
            (make-list (- N above) avg))))


(require quickcheck)

(define addends-quickcheck
  (property ([C (choose-integer 1 1000)] [N (choose-integer 1 1000)])
            (let ((as (addends C N)))
              (and (eq? (length as) N)
                   (eq? (apply + as) C)
                   (<= (- (apply max as) (apply min as)) 1)))))

(quickcheck addends-quickcheck)