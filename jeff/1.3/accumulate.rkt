; Getting tired of writing this out...
(define (print line) (display line) (newline))

; Exercise 1.32
; We'll start with the painfully abstract (accumulate) procedure.
; Again, first we'll use the recursive method,
; and overwrite it with the more efficient iterative method.
; I've verified that both of these work.
; (At least for these trivial examples...)
(define (accumulate combiner null-value term a next b)  ; Recursive.
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b))))  ; Whew!

(define (accumulate combiner null-value term a next b)  ; Iterative.
    (define (iter-accumulate a result)
        (if (> a b)
            result
            (iter-accumulate (next a) (combiner (term a) result))))
    (iter-accumulate a null-value))

(define (sum term a next b)
    (accumulate + 0 term a next b))

(define (product term a next b)
    (accumulate * 1 term a next b))

; And, a couple of trivial applications.
; To prove it actuallly works...
; First, an arithmetic series:
(define (identity x) x)
(define (inc x) (+ x 1))
(define (arithmetic a b)
    (sum identity a inc b))

; These should be equal:
(print (/ (* 100 (+ 100 1)) 2))
(print (arithmetic 1 100))

; Then, a simple factorial:
(define (factorial n)
    (product identity 1 inc n))

(print (* 6 5 4 3 2 1))
(print (factorial 6))

; Exercise 1.33
; I'm only going to do this one iteratively,
; partly because I find the filtering more intuitive in the iterative case.
; By now it's probably obvious that it's trivial to transform one to the other.
(define (filtered-accumulate filterer combiner null-value term a next b)
    (define (filtered-term a) (if (filterer a) (term a) null-value))
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (filtered-term a) result))))
    (iter a null-value))

; First, the sum-of-squares problem.
; Not too hard...
(define (prime? n)
    (define (smallest-divisor test)
        (cond
            ((> (square test) n) n)
            ((= (remainder n test) 0) test)
            (else (smallest-divisor (+ test 1)))))
    (= (smallest-divisor 2) n))
(define (square x) (* x x))
(define (sum-squares-prime a b)
    (filtered-accumulate prime? + 0 square a inc b))

; These should be equal:
(print (+ 4 9 25))
(print (sum-squares-prime 2 5))

; Now the products of positive, relatively prime integers.
; (gcd) is already implemented in Racket, but whatever;
; I'm going to try to write it, and justify it.
; Suppose g is a common divisor of a and b. Then:
;   a = bq + r
;       => (a'g) = (b'g)q + r
;       => r = g(a' - b'q)
; That is, g divides a and b => g divides r.
; Since b is smaller than a, we can continue executing:
;   (gcd b r)
; until we find something that exactly divides b and r,
; which => that it divides a and b.
; (I suppose it remains to be proven that it's the *greatest* common divisor?)
(define (gcd a b) ; (a, b) not ordered.
    (if (= (min a b) 0)
        (max a b)
        (gcd (min a b) (remainder (max a b) (min a b)))))
(define (coprime? a b)
    (= (gcd a b) 1))
(define (product-all-positive-coprimes n)
    (define (coprime-to-n? a) (coprime? a n))
    (filtered-accumulate coprime-to-n? * 1 identity 1 inc (- n 1)))

; These should be equal:
(print (* 7 5 3 1))
(print (product-all-positive-coprimes 8))

