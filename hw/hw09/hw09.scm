(define (curry-cook formals body)
  (define (helper formals)
    (if (null? formals)
        body  ; 如果没有形式参数，则返回主体
        (let ((first (car formals))  ; 取第一个形式参数
              (rest (cdr formals)))   ; 取其余形式参数
          (list 'lambda (list first) (helper rest)))))  ; 创建嵌套 lambda

  (helper formals))  ; 调用辅助函数


(define (curry-consume curry args)
  (define (apply-args func args)
    (if (null? args)
        func  ; 如果没有更多参数，返回当前的函数
        (let ((next-arg (car args)))  ; 取出当前参数
          (apply-args (func next-arg) (cdr args)))))  ; 应用当前参数并递归处理剩余参数
          
  (apply-args curry args))  ; 开始应用参数


(define-macro (switch expr options)
  (switch-to-cond (list 'switch expr options)))

(define (cadr s) (car (cdr s)))
(define (caddr s) (car (cdr (cdr s))))
(define (switch-to-cond switch-expr)
  (cons 'cond  ; 第一部分是 cond
        (map (lambda (option)
               (cons (list 'equal? (cadr switch-expr) (car option))  ; 构建条件
                     (cdr option)))  ; 取出表达式部分
             (caddr switch-expr))))  ; 获取 options

