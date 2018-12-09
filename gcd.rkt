#!/usr/bin/racket
#lang racket/base


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


; 1.20
; gcd(206, 40) =>
; remainder(206, 40), gcd(40, 6) =>
; remainder(40, 6), gcd(6, 4) =>
; remainder(6, 4), gcd(4, 2) =>
; remainder(4, 2), gcd(2, 0) =>
; 2

; Theyre not different?  because b always needs to be evaluated, which is the remainder from the last one?

(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  		((divides? test-divisor n) test-divisor)
    	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))



; 1.21
;  199 - prime
;  1999 - prime
;  7 - not prime
