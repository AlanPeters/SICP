(define (fast-expt b n)
  (expt-iter b n 1)
  )

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((= n 1) (* a b))
        ((is-even n) (expt-iter (* b b) (/ n 2) a ))
        (else (expt-iter b (- n 1) (* a b)))
        )
  )

(define (is-even n) (= (remainder n 2) 0))




(define (addone a) (+ a 1))
