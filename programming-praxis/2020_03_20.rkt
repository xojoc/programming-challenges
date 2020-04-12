#lang racket
;https://programmingpraxis.com/2020/03/20/homework-2/

(define (count-up-down n)
  (let ((c (* 2 (- n 1))))
    (for ((i (range (+ c 1))))
      (if (< i n)
          (printf "~a " (+ 1 i))
          (printf "~a " (- (+ c 1) i))))))

(count-up-down 1)
(newline)
(count-up-down 5)
(newline)
(count-up-down 9)