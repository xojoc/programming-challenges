#lang racket
; https://programmingpraxis.com/2020/03/17/csv-to-html/

(require scribble/html
         csv-reading)

(define (csv->html-table in-port)
  (table
   (for/list ([row (in-producer (make-csv-reader in-port) '())]
              [i (in-naturals)])
     (let ((is-header (= i 0)))
       (tr (map (if is-header th td) row))))))

(define (csv->html in-port out-port)
  (output-xml (list (doctype "html")
                    (html
                     (head (title "CSV to HTML"))
                     (body (csv->html-table in-port))))
              out-port))

(csv->html (open-input-string "color,city,email,gender,ip address
Yellow,<p>try escape</p>,dcaldeiro0@altervista.org,Female,156.96.111.112
Yellow,Victoria,mgrigorian1@lulu.com,Male,49.152.41.70
Purple,Guihulñgan,mbonnier4@hexun.com,Female,136.198.31.80
Teal,Karangpete,clomax11@ocn.ne.jp,Male,") (current-output-port))