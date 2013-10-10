#lang racket

(require racket/draw)

(define target (make-bitmap 700 700))
(define dc (new bitmap-dc% (bitmap target)))
(send dc clear)
(send dc set-pen (make-color 0 100 0) 1 'solid)

; http://en.wikipedia.org/wiki/Barnsley_fern
;
; The first point is 0,0
; New points are iteratively computed by randomly applying one of the
; following four coordinate transformations:
;
;ƒ1 probability: 1%
;    x(n + 1) = 0
;    y(n + 1) = 0.16 y(n)
;
; ƒ2 probability: 85%
;    x(n + 1) = 0.85 x(n) + 0.04 y(n)
;    y(n + 1) = −0.04 x(n) + 0.85 y(n) + 1.6
;
;ƒ3 probability: 7%
;    x(n + 1) = 0.2 x(n) − 0.26 y(n)
;    y(n + 1) = 0.23 x(n) + 0.22 y(n) + 1.6
;
;ƒ4 probability: 7%
;    x(n + 1) = −0.15 x(n) + 0.28 y(n)
;    y(n + 1) = 0.26 x(n) + 0.24 y(n) + 0.44
;
; Note that the complete fern is within the range
;   −2.1820 < x < 2.6558 and 0 ≤ y < 9.9983.

; define some ranges for the probability
(define range1 '(0 . 0))
(define range2 '(1 . 85))
(define range3 '(86 . 92))
(define range4 '(93 . 99))

(define (f1 x y)
  (cons 0 (* y 0.16)))

(define (f2 x y)
  (cons (+ (* x 0.85) (* y 0.04)) (+ (* x -0.04) (* y 0.85) 1.6)))

(define (f3 x y)
  (cons (+ (* x 0.2) (* y -0.26)) (+ (* x 0.23) (* y 0.22) 1.6)))

(define (f4 x y)
  (cons (+ (* x -0.15) (* y 0.28)) (+ (* x 0.26) (* y 0.24) 0.44)))

(define (fern x y iter)
  (define chance (random 100))
    (if (= iter 0) "Klaar"
      (let ((new-xy (cond
           ((and (>= chance (car range1)) (<= chance (cdr range1))) (f1 x y))
           ((and (>= chance (car range2)) (<= chance (cdr range2))) (f2 x y))
           ((and (>= chance (car range3)) (<= chance (cdr range3))) (f3 x y))
           ((and (>= chance (car range4)) (<= chance (cdr range4))) (f4 x y)))))
      (begin
         (send dc draw-point (+ (* (car new-xy) 70) 350) (+ (* (cdr new-xy) -60) 650))
         (fern (car new-xy) (cdr new-xy) (- iter 1))))))

(fern 0 0 10000) ; parameters: x y and number of iterations
(send target save-file "fern.png" 'png)

