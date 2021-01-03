#lang racket
; https://programmingpraxis.com/2020/11/10/memfrob/

(define (memfrob str)
  (bytes->string/utf-8 (list->bytes
                        (map (λ (b) (bitwise-xor b 42))
                             (bytes->list (string->bytes/utf-8 str))))))

(memfrob "xojoc")
(memfrob (memfrob "xojoc"))
