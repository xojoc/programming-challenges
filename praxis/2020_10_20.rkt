#lang racket
; https://programmingpraxis.com/2020/10/20/doubled-pairs/

; Assuming the numbers are already sorted

(define (search-double v i a)
  (let l ([j (+ i 1)])
    (if (or
         (>= j (vector-length v))
         (> j (+ i a)))
        null
        (let ([b (vector-ref v j)])
          (if (= (* a 2) b)
              (cons a b)
              (l (+ 1 j)))))))

(define (doubled-pairs v)
  (let l ([pairs '()]
          [i 0])
    (if (>= i (vector-length v))
        (reverse pairs)
        (let ([a (vector-ref v i)])
          (if (>= (+ i a) (vector-length v))
              (reverse pairs)
              (l (cons (search-double v i a) pairs) (+ i 1)))))))

(doubled-pairs #[1 2 3 4])
(doubled-pairs #[1 3 5 7 9])
              
              
      
