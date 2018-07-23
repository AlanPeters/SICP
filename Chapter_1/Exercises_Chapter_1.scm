
;Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0 ) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))   ;p'
                   (+ (* 2 p q) (* q q)) ;q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
#|
a <- bq + aq + ap
b <- bp + aq

b' <- (bp + aq)p + (bq + aq + ap)q
b' <- bp^2 + aqp + bq^2 + aq^2 + apq
b' <- bp^2 + 2aqp + bq^2 + aq^2
;b' <- b(p') + a(q')
b' <- b(p^2 + q^2) + a(2qp + q^2)

a' <-(bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
a' <- bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2
a' <- 2bpq + 2aq^2 + bq^2 + 2apq + ap^2
;a' <- b(q') + a(q') + a(p')
a' <- b(2pq + q^2) + a(2pq + q^2) + a(q^2 + p^2)

q' <- 2pq + q^2
p' <- q^2 + p^2

|#

(define (f g)
  (g 2))

(define (square x)
  (* x x))


(define tolerance 0.00001)
;Exercise 1.35
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio) (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

;Exercise 1.36
(define (fixed-point-with-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (exercise_1_36) (fixed-point-with-print (lambda (x) (/ (log 1000) (log x)) ) 5))

                                        ;Exercise 1.37
(define (cont-frac n d k)
  (define (inner-frac n d k i)
    (if (= k 1)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (inner-frac n d (- k 1) (+ i 1))
              )
           )
        )
    )
  (inner-frac n d k 1)
  )

(define (exercise_1_37)
  (/ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                1000)
     )
  )

                                        ;Exercise 1.38
(define (euler-list i)
  (let
      ((n (+ i 1)))
    (if (= 0 (modulo n 3))
        (* (/ n 3) 2)
        1
        )))

(define (euler-list-test k)
   (if (> k 1)
       (euler-list-test (- k 1)))
   (display (euler-list k))
  )

(define (exercise_1_38)
  (cont-frac (lambda (i) 1.0)
             euler-list
             1000)
  )

                                        ;Section 1.3.4

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
         (/ (- (g (+ x dx)) (g x))
            dx)))

(define (cube x) (* x x x))

(define (average x y)
  (/ ( + x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt2 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
                                        ;Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c )))

(define (exercise_1_40)
  (newtons-method (cubic -3 -1 3) 4))

(define (inc x)
  (+ x 1))

(define (double g)
  (lambda (x) (g ( g x))))

(define (exercise_1_41)
  (((double (double double)) inc) 5))


(define (compose f g)
  (lambda (x) (f (g x))))

(define (exercise_1_42)
  ((compose square inc) 6)
  )

(define (repeated f x)
  (lambda (n)
    (if (= x 1)
        (f n)
        ((compose f (repeated f (- x 1)))  n ))
        ))

(define (exercise_1_43)
  ((repeated square 2) 5 )
  )

(define (smooth f)
(let ((dx 0.1))
  (lambda (x)
    (/ (+
        (f (- x dx))
        (f x)
        (f (+ x dx))
        ) 3))))

(define (exercise_1_44)
((smooth (lambda (x)
           (square (- x 5))
           )
         ) 5 )
)
