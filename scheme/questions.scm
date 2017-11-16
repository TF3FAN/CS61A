(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (cons first lst)) rests)
)

(define (zip pairs)
  (if (null? (car pairs)) 
    nil
    (cons (cons (caar pairs) 
      (cons (car (cadr pairs)) nil)) 
        (zip (cons (cdar pairs) 
                (cons (cdr (cadr pairs)) nil)))))
)
;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (create_range lst strt)
      (if (null? lst) 
        nil
        (cons strt (create_range (cdr lst) (+ strt 1))))
  )
  (zip (cons (create_range s 0) (cons s nil)))
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((= total 0) (cons nil nil))
        ((< total 0) nil)
        ((null? denoms) nil)
        (else (append (cons-all (car denoms) 
          (list-change (- total (car denoms)) denoms)) 
            (list-change total (cdr denoms))))
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ;(cons 'lambda (cons params (cons (map (let-to-lambda) body)) nil))
           (list 'lambda params (map (let-to-lambda) body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ;(cons (cons 'lambda (cons (car (zip values)) (cons (car body) nil))) (cadr (zip values)))
           (cons (list 'lambda (car (zip values)) (let-to-lambda (car body))) (cadr (zip values)))
      
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )))
