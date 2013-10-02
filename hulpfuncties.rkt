;********************************************************************
;       (c) Copyright 2013, Hogeschool voor de Kunsten Utrecht
;                       Hilversum, the Netherlands
;********************************************************************
;
; File name	: hulpfuncties.rkt
; System name	: INF
;
; Description   : Racket Scheme functions for manipulating music data
;
; Authors       : Marc Groenewegen, Daan van Hasselt
; E-mail        : marc.groenewegen@kmt.hku.nl, daan.vanhasselt@kmt.hku.nl
;
;*********************************************************************
;
; Convenience functions for manipulating music data
;
; Exports procedures:
;  (exporteer-melodie melodie [nootlengte 4])
;  (transponeer-stamtoon toon afstand)
;  (transponeer-stamtonen lst afstand)
;
; Examples: at end of file
;
;*********************************************************************

#lang racket

(provide exporteer-melodie)
(provide transponeer-stamtoon)
(provide transponeer-stamtonen)

; melodie is flattened to offer the possibility of entering musical
;  structures of arbitrary complexity containing only note symbols.
;  The hierarchy may be visually informative to the composer, but in the end
;  we export a linear representation of all tree leaves.
(define (exporteer-melodie melodie [nootlengte 4])
  (cons 'serial
   (for/list ((note-pitch (flatten melodie)))
     (if (or (equal? note-pitch 'r) (equal? note-pitch 'nap))
       (list 'nap (dotnote nootlengte))
       (list 'note (+ (note-to-number note-pitch) 60) (dotnote nootlengte))))))
   

; dotnote is used for dotted notes which have a duration of 3/2 times the
;  indicated integer.
(define (dotnote n)
  (if (flonum? n) (* 2/3 (inexact->exact n)) n))

;;
;; translate absolute note name to number
;;
(define (note-to-number note)
 (let* ((raised-names (vector 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b))
         (lowered-names (vector 'c 'des 'd 'es 'e 'f 'ges 'g 'as 'a 'bes 'b))
         (result (vector-member note raised-names)))
   ;; if not found in the first vector the result is #f and we'll look in
   ;;   the second list
   (if (not result) (vector-member note lowered-names)
       result)))

;
; symbolic transpose of natural note (one note only)
;
(define (transponeer-stamtoon toon afstand)
  (let* ((stamtonen (vector 'c 'd 'e 'f 'g 'a 'b))
        (ref (vector-member toon stamtonen)))
     (if (equal? toon 'r) 'r
       (if (not ref) #f ; not found
        (vector-ref stamtonen (modulo (+ ref afstand) (vector-length
	stamtonen)))))))

; symbolic transpose of a list of natural notes
(define (transponeer-stamtonen lst afstand)
  (if (empty? lst) '()
      (cons (transponeer-stamtoon (car lst) afstand) (transponeer-stamtonen (cdr lst) afstand))))

;
; examples
;
; (exporteer-melodie '(des c g a bes bes a bes g es f g es d d c) 8)
;
; (transponeer-stamtoon 'c 0)
; (transponeer-stamtoon 'c 1)
; (transponeer-stamtoon 'c 8)
; (transponeer-stamtoon 'e 2)
; (transponeer-stamtoon 'e -1)
; (transponeer-stamtoon 'e -9)
; (transponeer-stamtoon 'k 2) ; returns #f because 'k is not found

