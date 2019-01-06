; This is the "naive" prime algorithm based on divisors.
; This runs in O(sqrtN) time.
(define (prime? n)
    (= (smallest-divisor n) n))
(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n test)
    (define (square x) (* x x))
    (cond
        ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (find-divisor n (+ test 1)))))

; (runtime) doesn't actually exist in Racket.
; I've implemented it here, somewhat naively...
(define (microseconds) (* 1000 (current-inexact-milliseconds)))
(define THEN (microseconds))
(define (runtime)
    (- (microseconds) THEN))

; These are the various printing functions.
; I've amended these somewhat so that we *only* print primes, not composites.
; The composites clutter up the output.
(define (timed-prime-test n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))
        false))
(define (report-prime n elapsed-time)
    (display n)
    (display " *** ")
    (display elapsed-time)
    (newline)
    true)

; Exercise 1.22
; And, here, is the actual exercise.
; I'm not testing only odd integers here, which is wasteful,
; but it doesn't affect the output.
(define (find-primes gt k)
    (cond
        ((= k 0) (void))
        ((timed-prime-test (+ gt 1)) (find-primes (+ gt 1) (- k 1)))
        (else (find-primes (+ gt 1) k))))

(display "SLOW:")
(newline)
(find-primes 1000 3)
(find-primes 10000 3)
(find-primes 100000 3)
(find-primes 1000000 3)
(newline)

; My results:
;
; 1009 *** 1.0
; 1013 *** 1.0
; 1019 *** 6.0
; 10007 *** 2.0
; 10009 *** 3.0
; 10037 *** 2.0
; 100003 *** 7.0
; 100019 *** 8.0
; 100043 *** 7.0
; 1000003 *** 23.0
; 1000033 *** 23.0
; 1000037 *** 23.0
;
; sqrt(10) ~= 3 so this isn't too bad,
; especially the jump from 1e5 -> 1e6.

; Exercise 1.23
; This is an improvement to the find-divisor routine:
; Obvioiusly, if a number is not divisible by 2, it's not divisible by 2k.
; This seems like a "weak" form of something more general:
; for any prime p, there is no need to consider kp.
(define (next i) (if (= i 2) 3 (+ i 2)))
(define (find-divisor n test)
    (define (square x) (* x x))
    (cond
        ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (find-divisor n (next test)))))

(display "NO EVEN:")
(newline)
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(newline)

; My results:
;
; 1009 *** 35.0  <-- Wtf! Can't reproduce this interactively, some penalty...
; 1013 *** 2.0
; 1019 *** 2.0
; 10007 *** 3.0
; 10009 *** 3.0
; 10037 *** 3.0
; 100003 *** 6.0
; 100019 *** 6.0
; 100043 *** 7.0
; 1000003 *** 17.0
; 1000033 *** 16.0
; 1000037 *** 16.0
;
; Only an improvement for >= e5.
; 23/16 ~= 1.4 for 1e6.
; It's not 2 because we're only testing up to sqrt(n).
; So, we *had* performance p=sqrt(n).
; Now, we *have* performance sqrt(n/2)=sqrt(1/2)sqrt(n)=sqrt(1/2)p~=.7p.
; Which is actually very nearly what we have...
; So I think we're good!
; (Is that naive though?)

; This is the "fast prime" method,
; which gets O(lgn) performance at the expense of probabilistic output.
; (Our confidence increases with the number of trials)
(define (expmod base expo m)
    (define (square x) (* x  x))
    (cond
        ((= expo 0) 1)
        ((even? expo)
            (remainder (square (expmod base (/ expo 2) m)) m))
        (else
            (remainder (* base (expmod base (- expo 1) m)) m))))
(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
    (cond
        ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.24
; Let's use the O(lgn) fast-prime? in place of the O(sqrtn) prime?
; The choice of trials here is arbitrary but it *will* affect the runtime!
(define (start-prime-test n start-time)
    (if (fast-prime? n 5)
        (report-prime n (- (runtime) start-time))
        false))

(display "FAST:")
(newline)
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(newline)

; My results:
;
; 1009 *** 85.0 <-- This simply must be some penalty for overwriting a function.
; 1013 *** 6.0
; 1019 *** 6.0
; 10007 *** 6.0
; 10009 *** 6.0
; 10037 *** 9.0
; 100003 *** 7.0
; 100019 *** 7.0
; 100043 *** 7.0
; 1000003 *** 8.0
; 1000033 *** 8.0
; 1000037 *** 11.0
;
; p=log(n) -> log(10n)=log(10)+log(n)~=3+p
; So this one is a bit harder to square with expectation...
; Although, obviously, the order of growth is less "harsh" than the root case.

; Exercise 1.25
; I've actually tested this in Python, but not in Racket;
; my experience is that it's a recpice for a hanging interpreter!
; Since a^n grows *quite* large,
; I think the assumption of O(1) * and / is violated.
; Assume 64-bit registers: 2^64 is the maximum size of an unsigned int.
; Let a=2, n=1000037: we've overshot a register by circa 1e5.
; (expt 2 128) does not return a negative number,
; so I'm assuming Racket does not impose a maximum integer
; and "spills out" into additional registers.
; A language like C might return a wrong answer in reasonable time.

; Exercise 1.26
; Our log implementation is represented by a simple recurrence:
;     T(n) = T(n/2) + O(1)  => O(logn)
; Louis' is pernicious:
;     T(n) = 2T(n/2) + O(1) => O(n)
; It's sort of like sub-structure: we are caching the redundant T(n/2),
; while Louis is calculating it explicitly.
; Consider a recursion tree. For n nodes, Louis does:
;     1 + 2 + 4 + 8 + ... + 2^(logn) = 2n - 1
; work.

