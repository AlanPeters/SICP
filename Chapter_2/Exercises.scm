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

                                        ;Exercise 2.21
(define (square n)
  (* n n))

(define (square-list1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (exercise_2_21a)
  (square-list1 (list 1 2 3 4)))

(define (exercise_2_21b)
  (square-list2 (list 1 2 3 4)))

                                        ;Exercise 2.23
(define (for-each2 f items)
  (cond ((not (null? items))
         (f (car items))
         (for-each2 f (cdr items)))))

(define (for-each proc list)
  (cond
   ((null? list) '())
   (else (proc (car list))
         (for-each proc (cdr list)))))

(define (exercise_2_23)
  (for-each2 (lambda (x) (display x) (newline))
            (list 57 321 88 )))

                                        ;Exercise 2.24
(define (exercise_2_24)
  (list 1 (list 2 (list 3 4))))

                                        ;Exercise 2.25
(define (exercise_2_25a)
  (car (cdaddr (list 1 3 (list 5 7) 9))))

(define (exercise_2_25b)
  (caar (list (list 7))))

(define (exercise_2_25c)
  (cadadr(cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))

                                        ;Eercise 2.30-1
(define (map-tree tree function)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree sub-tree function)
             (function sub-tree)))
       tree))
(define (map-tree_2 tree function)
  (cond ((null? tree) '())
        ((not (pair? tree)) (function tree))
        (else (cons (map-tree_2 (car tree) function)
                    (map-tree_2 (cdr tree) function)))))

(define (square-tree tree)
  (map-tree tree square))

(define (square-tree_2 tree)
  (map-tree_2 tree square))

(define (exercise_2_30)
  (let ((test-list (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7))))
    (display
     (square-tree test-list))
    (newline)
    (display
     (square-tree_2 test-list))
    (newline)
    ))

                                        ;Exercise 2.32
(define (subsets s)
  (define (inner-func x)
    (lambda (y)
      (if (not (null? x))
          (append y (list x))))
          )
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (inner-func (car s)) rest)))))

                                        ;Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (exercise_2_33_a)
  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
  (map square (list 1 2 3))
  )
(define (exercise_2_33_b)
  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))
  (append (list 1 2 3) (list 4 5 6)))
(define (exercise_2_33_c)
  (define (length sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))
  (length (list 1 2 3)))
