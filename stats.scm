(use srfi-1)

(define (stat-bonus stat)
  (inexact->exact (floor (/ (- stat 10) 2))))

(define (roll-stat)
  (let*
    ([lst (list (d6) (d6) (d6) (d6))]
     [lowest (apply min lst)])
       (let-values ([(low high) (partition (lambda (x) (= x lowest)) lst)])
         (if (< 1 (length low))
           (apply + (append high (cdr low)))
           (apply + high)))))


