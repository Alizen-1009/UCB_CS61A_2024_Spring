(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (help idx a)
    (if (null? a) nil
      (cons (list idx (car a)) (help (+ idx 1) (cdr a)))
    )
  )  
  (help 0 s)
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists S1 and S2 according to ORDERED? and return
;; the merged lists.
(define (merge ordered? s1 s2)
  ; BEGIN PROBLEM 16
  (cond 
    ((null? s1) s2)
    ((null? s2) s1)
    (else
      (if (ordered? (car s1) (car s2))
        (cons (car s1) (merge ordered? (cdr s1) s2))
        (cons (car s2) (merge ordered? s1 (cdr s2)))
      )
    )
  )
)
  ; END PROBLEM 16

;; Optional Problem 2

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr )
        ((quoted? expr) expr)
        ((or (lambda? expr)
             (define? expr))
          (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
            (cons form (cons params (let-to-lambda body)))
          )
        )
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN OPTIONAL PROBLEM 2
            (define tmp (zip values))
            (cons (list 'lambda (car tmp) (let-to-lambda (car body))) (map let-to-lambda (cadr tmp)))
           ; END OPTIONAL PROBLEM 2
           ))
        (else
         ; BEGIN OPTIONAL PROBLEM 2
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END OPTIONAL PROBLEM 2
         )))

; Some utility functions that you may find useful to implement for let-to-lambda

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))
