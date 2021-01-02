#lang racket
; https://programmingpraxis.com/2020/03/06/squares-of-a-sorted-array/

(define (sorted-squares lst)
  (map (curryr expt 2)
       (let-values ([(negatives positives) (splitf-at lst negative?)])
         (let loop ((positives positives)
                    (negatives (reverse negatives))
                    (abs-sort '()))
           (cond
             [(empty? negatives) (append abs-sort positives)]
             [(empty? positives) (append abs-sort negatives)]
             [else
              (if (< (first positives) (abs (first negatives)))
                  (loop (rest positives) negatives (append abs-sort (list (first positives))))
                  (loop positives (rest negatives) (append abs-sort (list (first negatives)))))])))))
           
    
(sorted-squares '(-11 -5 -4 -1 0 3 10))