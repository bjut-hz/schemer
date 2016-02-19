(define (square x)
  (* x x))

;;求值为奇数的叶子的平方和
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define input (list 1 (list 2 (list 3 4)) 5))
(sum-odd-squares input)

;;tail recursion,尾递归
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))



;;构造出所有偶数的fib的一个表
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;;test
(even-fibs 8)

;;============================================================
;;我们注意到sum-odd-squares和even-fibs处理步骤分别是：
;;enumerate: tree, leaves -> filter: odd? -> map: square -> accumulate: +, 0
;;enumerate: int -> map: fib ->filter: even? -> accumulate: cons, ()
;;分别包含枚举，映射map，过滤器以及累积函数，因此可以提取更高的抽象函数：filter，map以及accumulate
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (cond
    ((null? sequence) initial)
    ((op (car sequence) (accumulate op initial (cdr sequence))))))

(define (enumerate-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (cond
    ((< high low) '())
    (else (cons low (enumerate-interval (+ low 1) high)))))

;;高级别抽象，分别实现sum-odd-squares和even-fibs
(define (HZ-sum-odd-squares tree)
  (accumulate + 0
              (map square (filter odd? (enumerate-tree tree)))))
(define (HZ-even-fibs n)
  (accumulate cons '()
              (filter even? (map fib (enumerate-interval 0 n)))))

(HZ-sum-odd-squares input)
(HZ-even-fibs 8)
