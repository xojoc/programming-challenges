#lang racket
; https://programmingpraxis.com/2020/03/13/perfect-shuffle/

(define (faro-shuffle deck)
  (let* ((l (length deck))
         (h (quotient l 2)))
    (map (λ (i)
           (let ((j (quotient i 2)))
             (list-ref deck (if (even? i) j (+ h j)))))
         (range l))))

(define (count-shuffles deck-size)
  (let ((deck (range deck-size)))
    (let loop ((c 1)
               (shuffled-deck (faro-shuffle deck)))
      (if (equal? deck shuffled-deck)
          c
          (loop (+ c 1) (faro-shuffle shuffled-deck))))))

(count-shuffles 8)
(count-shuffles 52)
(count-shuffles 54)