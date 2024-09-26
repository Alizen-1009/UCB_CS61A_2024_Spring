(define (cadr s) (car (cdr s)))
(define (ascending? s)
    (define (helper lst prev)
        (cond
            ((null? lst) #t)
            ((null? (cdr lst)) #t)
            ((> (car lst) (cadr lst)) #f)
            (else (helper (cdr lst) (car lst)))
        )
    )
    (helper s '(-inf. -inf.))
)

(define (my-filter pred s)
    (cond
        ((null? s) '())
        ((pred (car s)) 
        (cons (car s) (my-filter pred (cdr s))))
        (else (my-filter pred (cdr s)))
    )
)

(define (interleave lst1 lst2) 
    (cond
        ((and (null? lst1) (null? lst2)) '())
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cons (car lst1) 
                    (cons (car lst2) 
                            (interleave (cdr lst1) (cdr lst2)))))))

(define (member item lst)
  (cond
    ((null? lst) #f)               
    ((equal? item (car lst)) #t)   
    (else (member item (cdr lst))))) 

(define (reverse lst)
  (define (reverse-helper lst acc)
    (if (null? lst)
        acc
        (reverse-helper (cdr lst) (cons (car lst) acc))))
  (reverse-helper lst '()))

(define (no-repeats s)
  (define (helper s seen)
    (cond
      ((null? s) (reverse seen))

      ((member (car s) seen) (helper (cdr s) seen))
      (else (helper (cdr s) (cons (car s) seen)))))
  
  (helper s '()))

