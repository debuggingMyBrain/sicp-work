; This test fails for "Carmichael numbers", e.g. 561.
; For everybody *else*, many trials increases confidence in primality.
(define (fermat-prime? n trials)
    (define (expmod base expo m)
        (define (square x) (* x x))
        (cond
            ((= expo 0) 1)
            ((even? expo)
                (remainder (square (expmod base (/ expo 2) m)) m))
            (else
                (remainder (* base (expmod base (- expo 1) m )) m))))
    (define (test a)
        ; Since a < n, (= (remainder a n) a)
        (= (expmod a n n) a))
    (cond
        ((= trials 0) true)
        ((test (+ 1 (random (- n 1)))) (fermat-prime? n (- trials 1)))
        (else false)))

; Exercise 1.28
; Interstingly, this algorithm is very robust.
; Even with (= trials 1), this algorithm correct rejects a Carmichael number
; much more often than half of the time.
; I think this is because we check for a nontrivial square root
; *at every expmod invocation*, rather than for every a.
; Meaning, for a candidate N, we check log2(N) a<N for each call.
; Anyway...
(define (miller-rabin-prime? n trials)
    (define (foolproof-expmod base expo)
        (define (square x) (* x x))
        (define (nontrivial x)
            (define (check y)
                (cond
                    ((and
                        (not (= x 1))
                        (not (= x (- n 1)))
                        (= y 1)) 0)
                    (else y)))
            (check (remainder (square x) n)))
        (cond
            ((= expo 0) 1)
            ((even? expo)
                (nontrivial (foolproof-expmod base (/ expo 2))))
            (else
                (remainder (* base (foolproof-expmod base (- expo 1))) n))))
    (define (test a) (= (foolproof-expmod a (- n 1)) 1))
    (cond
        ((= trials 0) true)
        ((test (+ 1 (random (- n 1)))) (miller-rabin-prime? n (- trials 1)))
        (else false)))

(define (exhaust-primes n trials)
    (cond
        ((= n 1) null)
        (else
            (display n)
            (display " M=")
            (display (miller-rabin-prime? n trials))
            (display " F=")
            (display (fermat-prime? n trials))
            (newline)
            (exhaust-primes (- n 1) trials))))

(define (exhaust-carmichael trials)
    (define (iter-carmichael nums)
        (cond
            ((null? nums) null)
            (else
                (display (car nums))
                (display " M=")
                (display (miller-rabin-prime? (car nums) trials))
                (display " F=")
                (display (fermat-prime? (car nums) trials))
                (newline)
                (iter-carmichael (cdr nums)))))
    (iter-carmichael
        (list
            561
            41041 
            62745
            63973
            75361
            101101
            126217
            172081
            188461
            278545
            340561)))


(exhaust-primes 10 1)
(exhaust-carmichael 1)

