#lang racket
; https://programmingpraxis.com/2019/10/04/neatly-printing-a-csv-file/

(require csv-reading)

(struct csv-contents-properties (header body-rows columns-width line-number-width)
  #:transparent)

(define (format-cell cell is-header)
  ((if is-header string-titlecase identity)(string-trim cell)))

(define (get-columns-width row csw)
  (let ((lrow (length row))
        (lcsw (length csw)))
    (cond
      [(< lrow lcsw) (append (get-columns-width row (take csw lrow))
                             (list-tail csw lrow))]
      [(= lrow lcsw) (map (λ (r c) (max (string-length r) c)) row csw)]
      [(> lrow lcsw) (get-columns-width row (append csw (make-list (- lrow lcsw) 0)))])))

(define (collect-csv-contents-properties csv row)
  (let* ((header (csv-contents-properties-header csv))
         ; If the header is empty then this is the first row. Treat it as the header
         (is-header (null? header))
         (body-rows (csv-contents-properties-body-rows csv))
         (columns-width (csv-contents-properties-columns-width csv))
         (row (map (λ (cell) (format-cell cell is-header))
                   row)))
    (csv-contents-properties
     (if is-header row header)
     (if is-header body-rows (append body-rows (list row)))
     (get-columns-width row columns-width)
     (string-length (number->string (+ 1 (length body-rows)))))))

(define (total-pages body-rows lines-per-page)
  (+ 1 (quotient (- (length body-rows) 1) lines-per-page)))

(define (print-cells row columns-width out-port)
  (for ([c row]
        [i (in-naturals)])
    (display (~a c #:width (list-ref columns-width i)) out-port)
    (when (< (+ 1 i) (length row))
      (display "  ~|~  " out-port))))

(define (print-header header columns-width line-number-width out-port)
  (fprintf out-port "~a  " (~a "#" #:width line-number-width))
  (let ((header (if (= (length header) (length columns-width))
                    header
                    (append header (make-list (- (length columns-width) (length header)) "")))))
    (print-cells header columns-width out-port))
  (newline out-port))

(define (take-at-most lst len)
  (take lst (if (< (length lst) len) (length lst) len)))

(define (print-body body-rows columns-width line-number-width current-page lines-per-page out-port)
  (for ([row (take-at-most (drop body-rows (* current-page lines-per-page)) lines-per-page)]
        [ln (in-naturals (+ 1 (* current-page lines-per-page)))])
    (fprintf out-port "~a  " (~a ln #:width line-number-width #:align 'right #:pad-string "0"))
    (print-cells row columns-width out-port)
    (newline out-port)))

(define (print-potential-empty-lines body-rows current-page lines-per-page out-port)
  (when (= (+ 1 current-page) (total-pages body-rows lines-per-page))
    (for-each (λ (_) (newline out-port))
              (range (modulo (- (length body-rows)) lines-per-page)))))

(define (print-page-number body-rows columns-width line-number-width current-page lines-per-page out-port)
  (fprintf out-port "~a  Page ~a of ~a" (~a " " #:width line-number-width)
           (+ current-page 1)
           (total-pages body-rows lines-per-page)))

(define (print-page ccp current-page lines-per-page out-port)
  (let ((header (csv-contents-properties-header ccp))
        (columns-width (csv-contents-properties-columns-width ccp))
        (body-rows (csv-contents-properties-body-rows ccp))
        (line-number-width  (csv-contents-properties-line-number-width ccp)))
                 
    (print-header header columns-width line-number-width out-port)
    (newline out-port)
    (print-body body-rows columns-width line-number-width current-page lines-per-page out-port)
    (print-potential-empty-lines body-rows current-page lines-per-page out-port)
    (newline out-port)
    (print-header header columns-width line-number-width out-port)
    (newline out-port)
    (print-page-number body-rows columns-width line-number-width current-page lines-per-page out-port)))

   
(define (print-csv ccp out-port)
  (let* ((lines-per-page 7)
         (total-pages (total-pages (csv-contents-properties-body-rows ccp) lines-per-page)))
    (for [(p (in-range total-pages))]
      (print-page ccp p lines-per-page out-port)
      (when (< p (- total-pages 1))
        (newline out-port)
        (newline out-port)
        (newline out-port)
        (newline out-port)))))
  
(define (csv-print in-port out-port)
  (print-csv
   (sequence-fold collect-csv-contents-properties
                  (csv-contents-properties '() '() '() 0)
                  (in-producer (make-csv-reader in-port) '()))
   out-port))

(csv-print (open-input-string "color,city,email,gender,ip address
Yellow,Leifeng,dcaldeiro0@altervista.org,Female,156.96.111.112
Yellow,Victoria,mgrigorian1@lulu.com,Male,49.152.41.70
Orange,Yangqiao,vstanyon2@globo.com,Male,49.117.46.181
Aquamarine,Shuangshan,hleynagh3@4shared.com,Female,225.222.52.11
Purple,Guihulñgan,mbonnier4@hexun.com,Female,136.198.31.80
Turquoise,,,Female,125.251.52.240
Teal,Kalāt,mkembery6@parallels.com,Female,104.202.31.234
Puce,Inawashiro,mgarci7@geocities.jp,Female,196.155.200.119
Maroon,Chinameca,jbeeden8@gravatar.com,Male,238.158.27.120
Violet,Bayt Wazan,hrebichon9@examiner.com,Female,
Aquamarine,Nanqi,lpassiona@prnewswire.com,Male,131.97.38.128
Puce,Gusang,achatteyb@latimes.com,Female,248.112.149.123
Pink,Leping,hcollingec@senate.gov,Female,53.162.39.53
Turquoise,Baghlān,hpereirad@google.co.uk,Male,33.188.44.93
Indigo,Bahía Honda,,Male,216.176.146.181
Goldenrod,Nazran’,cserckf@techcrunch.com,Female,38.57.249.121
Yellow,Robonkon,achisholmg@si.edu,Female,
Fuscia,Huancaray,dpatesh@blog.com,Female,18.170.196.214
Red,Um Jar Al Gharbiyya,kstephensi@w3.org,Female,
Puce,Dzoraghbyur,,Male,209.232.114.147
Fuscia,Stobreč,dbroginik@simplemachines.org,Female,61.210.103.223
Yellow,Santa Praxedes,bmegaineyl@time.com,Female,163.110.138.121
Yellow,Aţ Ţawāḩīn,lrameletm@state.gov,Female,35.206.205.202
Puce,Itsandra,cantoniewiczn@harvard.edu,Male,111.189.36.5
Yellow,,fthorpeo@de.vu,Male,118.0.165.142
Red,Tonekābon,jproskep@geocities.com,Male,34.189.234.169
Green,Serzedo,ghainningq@harvard.edu,Male,173.118.146.42
Purple,Rönninge,bgoodsallr@xrea.com,Male,
Aquamarine,Viekšniai,mleahs@smugmug.com,Male,246.180.168.181
Blue,Dobropillya,kbenoist@state.tx.us,Female,
Yellow,‘Eilabun,,Female,95.47.136.198
Goldenrod,Kajiki,sbierv@npr.org,Female,138.246.73.60
Red,Richmond,wglenfieldw@tmall.com,Female,82.144.144.146
Pink,Bantarjati,lgiovannettix@berkeley.edu,Male,96.76.85.222
Purple,Gaozhou,krosenzveigy@sfgate.com,Male,
Maroon,Bilje,dmcsporrinz@oakley.com,Female,140.192.53.182
Teal,Ad Dabbah,guwins10@google.com.br,Male,71.211.1.176
Teal,Karangpete,clomax11@ocn.ne.jp,Male,") (current-output-port))