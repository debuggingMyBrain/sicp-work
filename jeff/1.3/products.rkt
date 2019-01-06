; Exercise 1.31
; First, the recursive method;
; then, we'll override it with the more efficient iterative method.
; I've verified that both of these work...
(define (product term a next b) ; Recursive.
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

(define (product term a next b) ; Iterative.
    (define (iter-product a result)
        (if (> a b)
            result
            (iter-product (next a) (* result (term a)))))
    (iter-product a 1))

; And, the factorial, trivially implemented via (product).
(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
    (product identity 1 inc n))

; And, John Wallis' approximation of pi, also implemented via (product),
; though much less trivially!
; It's tempting to use two variables: (numerator, denominator).
; But I believe, at any step, using only the term number, n:
;   numerator   = 2 * (n - quotient(max(0, n - 1) / 2))
;   denominator = 2 * (n - quotient(n / 2)) + 1
; This took me... quite a while to figure out.
(define (wallis-pi n)
    (define (wallis-term i)
        (/
            (* 2.0 (- i (quotient (max 0 (- i 1.0)) 2.0)))
            (+ 1.0 (* 2.0 (- i (quotient i 2.0))))))
    (* 4 (product wallis-term 1 inc n)))

(display "WALLIS:")
(newline)
(display (wallis-pi 1))
(newline)
(display (wallis-pi 100))
(newline)
(display (wallis-pi 1000))
(newline)
(display (wallis-pi 10000))
(newline)
(display (wallis-pi 100000))
(newline)
(display (wallis-pi 1000000))
(newline)
