#lang racket
; https://programmingpraxis.com/2020/11/17/sales-commission/

(define (calculate-commissions sales)
  (map (λ (n) (+ 200 (* 0.10 n))) sales))

(define (read-sales [in (current-input-port)])
  (let l ([sales '()])
    (define line (read-line))
    (if (eof-object? line)
        sales  
        (let ((n (string->number line)))
          (if n
              (l (append sales (list n)))
              (begin
                (printf "Value \"~a\" is not a number.\n" line)
                (l sales)))))))

(let ((commissions (calculate-commissions (read-sales))))
  (displayln (string-join (map number->string commissions) ", "))
  (printf "Sum: ~a\n" (apply + commissions)))