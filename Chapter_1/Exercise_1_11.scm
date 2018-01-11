(define (f n )
  (cond ((< n 3 ) n)
        (else (+
               (f (- n 1))
               (* 2 (f (- n 2)))
               (* 3 (f (- n 3)))
               )
              )
        )
  )


(define (fr n )
  (cond
   ((< n 3) n)
   ( else ( infr 3 n 2 1 0 ))
   )
  )


(define (infr i n n1 n2 n3 )
  (cond
   ((= i n) (calcf n1 n2 n3))
   (else (infr (+ i 1) n (calcf n1 n2 n3) n1 n2))
   )
  )

(define (calcf n1 n2 n3)
  (+
   n1
   (* 2 n2)
   (* 3 n3)
   )
  )
