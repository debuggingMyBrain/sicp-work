; These are all from the text...
(define (sum-integers a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))

(define (cube x) (* x x x))
(define (sum-cubes a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))))

(display (sum-cubes 1 10))
(newline)

; -> pi/8, s.t. (* 8 (pi-sum a b)) approximates pi.
(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(display (pi-sum 1 10))
(newline)

; This one's just for fun!
; 1 - 1/3 + 1/5 - 1/7 + 1/9 - ... converges *very slowly* to pi.
; This is a a digression because the point of this section is abstraction,
; and not the discovery of new ways to evaluate pi!
; But, whatever...
(define (leibniz-pi terms)
    (define (next term)
        (if (= term terms)
            0
            (+ 
                (* (if (even? term) 1.0 -1.0) (/ 1.0 (+ (* 2.0 term) 1.0)))
                (next (+ term 1)))))
    (* 4 (next 0)))

(define (demonstrate-pi from-power to-power)
    (define (print-power power)
        (display "10^")
        (display power)
        (display ": ")
        (display (leibniz-pi (expt 10 power)))
        (newline)
        (if (< power to-power)
            (print-power (+ power 1))
            (void)))
    (display "LEIBNIZ:")
    (newline)
    (print-power from-power))

(demonstrate-pi 0 6)

; I have to be honest... I find this a little ugly.
(define (sum term a next b)
    (if (> a b)
        0
        (+
            (term a)
            (sum term (next a) next b))))

; But, I have to admit.
; This is a cool achievement!
(define (inc n) (+ n 1))
(define (identity i) i)
(define (sum-cubes a b)
    (sum cube a inc b))
; Obvious potential here for a function which returns an incrementer function.
; I assume we're not quite there yet in the text.
(define (pi-inc x) (+ x 4))
(define (pi-term x) (/ 1.0 (* x (+ x 2))))
(define (pi-sum a b)
    (sum pi-term a pi-inc b))

(display (sum-cubes 1 10))
(newline)
(display (pi-sum 1 10))
(newline)

; This is amazing, though....
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) 
       dx))

(display "INTEGRAL:")
(newline)
(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))
(newline)

; Exercise 1.29
; This is Simpson's Rule. Interestingly,
; (integral) seems to approach cube *from below*;
; (simpsons) seems to approach cube *from above*.
; I think that makes sense since Simpson's uses a quadratic function
; as its approximation over the h-interval, and cube is convex.
; (I ... think?)
; Also, (simpsons) looks less accurate which I don't expect.
(define (simpsons f a b n)
    (define h (/ (- b a) n))
    (define (simpsons-next x) (+ x 1))
    (define (simpsons-term x)
        (define (simpsons-constant)
            (cond
                ((= x a) 1.0)
                ((= x b) 1.0)
                ((even? x) 2.0)
                (else 4.0)))
        (* (simpsons-constant) (f (+ a (* x h)))))
    (* (/ h 3) (sum simpsons-term 0 simpsons-next n)))

(display "SIMPSONS:")
(newline)
(display (simpsons cube 0.0 1.0 100))
(newline)
(display (simpsons cube 0.0 1.0 1000))
(newline)

; Exercise 1.30
; Here, we rewrite (sum) as an iterative function.
; This is topical because I've noticed that (sum) fails for many terms.
(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

; Let's test it by rewriting the (leibniz-pi) procedure,
; and invoking it for very large powers of 10.
; This will hang for a bit because it's a long computation,
; but it won't crash by exhausting the physical memory!
(define (leibniz-term term)
    (*
        (if (even? term) 1.0 -1.0)
        (/ 1.0 (+ (* 2.0 term) 1.0))))
(define (leibniz-pi terms)
    (* 4 (sum leibniz-term 0 inc (- terms 1)))) ; Fenceposting 0-indexed terms.
(demonstrate-pi 0 7)

