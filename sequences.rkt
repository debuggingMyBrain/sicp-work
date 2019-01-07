#!/usr/bin/racket
#lang racket
(define (square x) (* x x))
; filter is already built in
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))


; 2.33
(define (map2 p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) null sequence))
; the op in accumulate gets 2 args, car and accum of cdr.  for map, just operate on the car
(map square (list 1 2 3 4 5))
(map2 square (list 1 2 3 4 5))

(define (append2 seq1 seq2)
	(accumulate cons seq2 seq1))
(append (list 1 2 3) (list 4 5 6))
(append2 (list 1 2 3) (list 4 5 6))

(define (length2 sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))
; hmm
; () -> 0, hits the null branch
; (1) -> f(1, 0) => 1 + 0 = 1
; (1 2) -> f(1 length2((2))) -> f(1, f(2, length2(()))) -> f(1, f(2, 0)) => f(1, (1+0)) -> f(1, 1) -> 1 + 1 = 2
(length (list 1 2 3 4 5))
(length2 (list 1 2 3 4 5))

; 2.34
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
		0
		coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))
; 1 + 6 + 5*8 + 32 = 79

(define (enumerate-tree tree)
	(cond ((null? tree) null)
			((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
			(enumerate-tree (cdr tree))))))
; 2.35
(define (count-leaves t)
	(accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
; enumerate tree flattens  tree into a list
; then we change that list into all 1s, and add em up
; had to look this up

; 2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons (accumulate op init (map (lambda (l) (car l)) seqs))
			(accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; 2.37
(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map (lambda (row) (dot-product row v)) m))
; matrix * vector is just dot product each row with vector
(define (transpose mat)
	(accumulate-n cons null mat))
; we want to take the first column and make it a first row
; thats basically what accumulate n does already
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
	(map (lambda (row) (matrix-*-vector cols row)) m)))
; M*N is basically dot product of each row with each col of N, aka each row of N transpose

; 2.38
; 3 / 2 / 1 -> 1.5
; 1 / 2 / 3 -> 1/6
; (1 (2 (3 ())))
; (((() 1) 2) 3)
; Commutativity I assume
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				(cdr rest))))
	(iter initial sequence))
; 2.39
(define (reverse-r sequence)
	(accumulate (lambda (x y) (append y (list x))) null sequence))
(reverse-r (list 1 2 3 4 5))
(define (reverse-l sequence)
	(fold-left (lambda (x y) (cons y x)) null sequence))
; does work, not 100% clear on it though
(reverse-l (list 1 2 3 4 5))

; 2.40
(define (enumerate-interval low high)
	(if (> low high)
		null
		(cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
	(accumulate append null (map proc seq)))
(define (unique-pairs n)
	(flatmap (lambda (x) (map (lambda (y) (list x y)) (enumerate-interval 1 x))) (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
	(map make-pair-sum
		)
