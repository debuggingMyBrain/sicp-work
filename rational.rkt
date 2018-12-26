#!/usr/bin/racket
#lang racket
(require racket/include)
(include "gcd.rkt")
;  section 2.1

; 2.1 (make it normalize the sign)
(define (make-rat n d)
	(let (
			(g (abs (gcd n d)))
			(norm-n (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0))) (abs n) (* -1 (abs n))))
			(norm-d (abs d))
		)
		(cons (/ norm-n g) (/ norm-d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
				(* (denom x) (denom y))))
(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
				(* (denom x) (denom y))))
(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
				(* (denom x) (denom y))))
(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
				(* (denom x) (numer y))))
(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		(* (numer y) (denom x))))

; 2.2
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)
(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))
(define (print-segment line)
	(newline)
	(print-point (start-segment line))
	(display "->")
	(print-point (end-segment line)))
(define (midpoint-segment line)
	(make-point 
		(/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
		(/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)
	)
)

; 2.3
; impl 1 - store top left and bottom right points
(define (make-rect top-left-point bottom-right-point)
	(cons top-left-point bottom-right-point)
)
(define (get-top-left rect) (car rect))
(define (get-bottom-right rect) (cdr rect))
(define (get-top-right rect) (make-point (x-point (get-bottom-right rect)) (y-point (get-top-left rect))))
(define (get-bottom-left rect) (make-point (x-point (get-top-left rect)) (y-point (get-bottom-right rect))))
(define (get-top-line rect) (make-segment (get-top-left rect) (get-top-right rect)))
(define (get-left-line rect) (make-segment (get-top-left rect) (get-bottom-left rect)))
(define (print-rect rect)
	(print-point (get-top-left rect))
	(print-point (get-top-right rect))
	(print-point (get-bottom-right rect))
	(print-point (get-bottom-left rect))
)

(define (get-length line)
	(sqrt (+ 
		(square (- (x-point (end-segment line)) (x-point (start-segment line)))) 
		(square (- (y-point (end-segment line)) (y-point (start-segment line))))
	)))
(define (get-area rect)
	(* (get-length (get-top-line rect)) (get-length (get-left-line rect))))
(define (get-perimeter rect)
	(* 2 (+ (get-length (get-top-line rect)) (get-length (get-left-line rect)))))

; impl 2 - store top and bottom line segments
(define (make-rect-2 top-line bottom-line) (cons top-line bottom-line))
(define (get-top-line-2 rect) (car rect))
(define (get-left-line-2 rect) (make-segment (start-segment (car rect)) (start-segment (cdr rect))))
;  etc

