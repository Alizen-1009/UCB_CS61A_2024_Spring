(define (over-or-under num1 num2) 
  (cond
   ((< num1 num2) -1)
   ((= num1 num2) 0)
   ((> num1 num2) 1)
  )
)

(define (make-adder num) 
  (lambda (inc)
    (+ num inc) 
  )
)

(define (composed f g) 
  (lambda (inc)
    (f (g inc))
  )
)

(define (repeat f n) 
  (lambda (x)
    (define (repeat-helper f n x)
      (if(= n 1)
        (f x)
        (repeat-helper f (- n 1) (f x))
      )
    )
    (repeat-helper f n x)
  )
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) 
  (if(= b 0)
      a
      (gcd b (modulo a b))
  )
)

