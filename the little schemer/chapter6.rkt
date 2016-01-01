(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (power n (sub1 m)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? axep))
      ((eq? (car (cdr axep)) (quote +))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr axep)) (quote *))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr axep)) (quote power))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) axep)
      ((eq? (car (cdr axep)) (quote +))
       (+ (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr axep)) (quote *))
       (* (value (car aexp)) (value (car (cdr (cdr aexp))))))
      ((eq? (car (cdr axep)) (quote power))
       (power (value (car aexp)) (value (car (cdr (cdr aexp)))))))))

;;format(+ 1 3)
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2rd-sub-exp
  (lambda (aexp)
    (cdr (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) axep)
      ((eq? (operator aexp) (quote +))
       (+ (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) (quote *))
       (* (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) (quote power))
       (power (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp)))))))