(define (abs x)
  (if (< x 0) (- x) x))
(define (average a b)
  (/ (+ a b) 2))
                                        ;Section 2.1
                                        ;Section 2.1.1
;; (define (make-rat n d )
  ;; (cons n d))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
                                        ;Exercise 2.1
(define (make-rat n d)
  (define (make-rat-inner n1 d1)
    (let ((g (abs (gcd n1 d1))))
      (cons (/ n1 g) (/ d1 g))))
  (if (and (< d 0))
      (make-rat-inner (- n) (- d))
      (make-rat-inner n d)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y )
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

                                        ;Exercise 2.2
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s))
                       )
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define (exercise_2.2)
  (print-point (midpoint (make-segment (make-point 1 1) (make-point 2 2))))
  )

                                        ;Exercise 2.4
(define (cons2 x y)
  (lambda (m) (m x y)))
(define (car2 z)
  (z (lambda (p q) p)))
#|
(car2 (cons2 1 2))
((cons2 1 2) (lambda (p q) p))
((lambda (m) (m 1 2)) (lambda (p q) p))
((lambda (p q) p ) (1 2))
1
|#
                                        ;Exercise 2.5
(define (cons3 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car3 z)
  (if (even? z)
      (+ 1 (car3 (/ z 2)))
      0))

(define (cdr3 z)
  (if (= (modulo z 3) 0)
      (+ 1 (cdr3 (/ z 3)))
      0))

                                        ;Exercise 2.17
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(define (exercise_2_17)
  (last-pair (list 23 72 149 34)))

                                        ;Exercise 2.18
(define (reverse list)
  (define (reverse-iter list tail)
    (if (null? (cdr list))
        (cons (car list) tail)
        (reverse-iter
         (cdr list)
         (cons (car list) tail))
        ))
  (reverse-iter list '()))

(define (exercise_2.18)
  (reverse (list 1 4 9 16 25)))

                                        ;Exercise 2.6
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))


;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (lambda (f) (lambda (x) x)) f) x))
;(lambda (f) (lambda (x) (f (lambda (x) x) x))
;(lambda (f) (lambda (x) (f x))

(define one
  (lambda (f) (lambda (x) (f x))))

;(lambda (f) (lambda (x) (f ((one f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (inc n)
  (+ n 1))

(define (print-church n)
  (display ((n inc) 0))
  (newline))

(define (add a b)
  (lambda (f)
    (lambda(x)
      ((a f) ((b f) x)))))

                                        ;Exercise 2.20

(define (same-parity . w)
  (define (parity-inner test? list)
    (cond ((null? list) '())
          ((test? (car list)) (cons (car list) (parity-inner test? (cdr list))))
          (else (parity-inner test? (cdr list)))))
  (if (even? (car w))
      (parity-inner even? w)
      (parity-inner odd? w)))

(define (exercise_2_20a)
  (same-parity 1 2 3 4 5 6 7 ))
(define (exercise_2_20b)
  (same-parity 2 3 4 5 6 7))
