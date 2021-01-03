#!/bin/sh
#|
# $Id$
# elvis: treccani		-- Dizionario Treccani
exec racket -u "$0" ${1+"$@"}
|#

#lang racket
; https://programmingpraxis.com/2020/11/06/surfraw/

; See https://docs.racket-lang.org/guide/scripts.html
; to understand how the above works.

; To install this elvi, copy the file to ~/.config/surfraw/elvi/treccani
; and make the file executable: chmod +x ~/.config/surfraw/elvi/treccani

(require (only-in browser/external send-url))

(define (build-url)
  (format "https://www.treccani.it/vocabolario/ricerca/~a/"
          (string-join (vector->list (current-command-line-arguments)) " ")))

(send-url (build-url) #f)