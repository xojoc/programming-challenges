#lang racket
;https://programmingpraxis.com/2009/02/19/rpn-calculator/

(require data/monad data/applicative data/either)
(require megaparsack megaparsack/text)

(define negative/p
  (do (char/p #\-)
    (pure -1)))
  
(define positive/p
  (do (or/p (char/p #\+)
            void/p)
    (pure 1)))

(define sign/p (or/p negative/p positive/p))

(define digits_separated_by_underscore/p
  (label/p "integer"
           (do [digits <- (many+/p (many+/p digit/p) #:sep (char/p #\_))]
             (pure (string->number (apply string (flatten digits)))))))

(define decimals/p
  (label/p "decimals"
           (do (or/p (try/p (do (char/p #\.)
                              [decimals <- digits_separated_by_underscore/p]
                              (pure decimals)))
                     (pure 0)))))

(define (number->decimal-part n)
  (define (count-digits n) (string-length (number->string n)))
  (/ n (expt 10 (count-digits n))))

(define number/p
  (label/p "number" 
           (do
               [sign <- sign/p]
             [integer <- digits_separated_by_underscore/p]
             [decimals <- decimals/p]
             (pure (* sign
                      (+ integer
                         (number->decimal-part decimals)))))))

(define operation/p
  (label/p "operation"
           (one-of/p '(#\+ #\- #\* #\/))))

(define (push n stack)
  (cons n stack))

(define (apply-operation op stack)
  (case op
    [("+") (push (+ (second stack) (first stack)) (list-tail stack 2))]
    [("-") (push (- (second stack) (first stack)) (list-tail stack 2))]
    [("*") (push (* (second stack) (first stack)) (list-tail stack 2))]
    [("/") (push (/ (second stack) (first stack)) (list-tail stack 2))]
    [("print") (begin (print stack) (newline) stack)]
    [else (printf "Error: unknown operation '~a'" op) stack]))


(define (get-next-word port)
  (let loop ((chars '()))
    (let ((ch (read-char port)))
      (cond
        [(eq? ch eof) (if (empty? chars) eof (list->string chars))]
        [(char-whitespace? ch) (if (empty? chars) (loop chars) (list->string chars))]
        [else (loop (append chars (list ch)))]))))

(define stack '())

(define (infinite-loop stack port)
  (let ((word (get-next-word port)))
    (if (eq? word eof)
        stack
        (let ((result (parse-string number/p word)))
          (match result
            [(failure _) (infinite-loop (apply-operation word stack) port)]
            [(success n) (infinite-loop (push n stack) port)])))))

(define (calculator) (infinite-loop '() (current-input-port)))

(require rackunit)

(define (p p s)
  (parse-result! (parse-string p s)))
(define tests
  (local[(define (pn s)
           (parse-result! (parse-string number/p s)))
         (define (calc stack input expected)
           (check-equal? (infinite-loop stack (open-input-string input)) expected))]
  
    (test-suite
     "Tests"
     (check-= (pn "99_27") 9927 0)
     (check-= (pn "-42_11") -4211 0)
     (check-= (pn "-42_11.") -4211 0)
     (check-= (pn "-42_11.23") -4211.23 0)
     (calc '(1 2 3) "+ +" '(6))
     (calc '(4 2) "/" '(1/2))
     (calc '() "19 2.14 + 4.5 2 4.3 / - *" '(1/2))
     )))


(require rackunit/text-ui)
 
(run-tests tests)