(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (plus n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (sub n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (x n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))