                                        ;Section 2.1
                                        ;Section 2.1.1
;; (define (make-rat n d )
  ;; (cons n d))
(define (abs x)
  (if (< x 0) (- x) x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

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