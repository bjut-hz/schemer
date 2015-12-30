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

;;tup1 and tup2 must have the same length
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((or (null? tup1) (null? tup2)) (quote()))
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define tup++
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup++ (cdr tup1) (cdr tup2)))))))

(define tup1 (cons 4 (cons 6 (cons 8 (cons 1 (quote()))))))
(define tup2 (cons 3 (cons 7 (quote()))))
;;(tup++ tup1 tup2)

(define great?
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (great? (sub1 n) (sub1 m))))))

(define less?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (less? (sub1 n) (sub1 m))))))

(define equalNum?
  (lambda (n m)
    (cond
      ((great? n m) #f)
      ((less? n m) #f)
      (else #t))))

(define power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (power n (sub1 m)))))))

(define division
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (division (- n m) m))))))

;;get the length of a lat
(define HZlength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (HZlength (cdr lat)))))))

;;get the nth element of the lat
(define pick
  (lambda (n lat)
    (cond
      ((null? lat) (quote()))
      ((zero? (sub1 n)) (car lat)) 
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (= 1 n)))