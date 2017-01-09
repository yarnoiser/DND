(use miscmacros (srfi 1 95) coops)

(define *stat-symbols* '(str dex con int wis cha))

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

;; list of starting stat numbers
(define (stats-list)
  (list (roll-stat)
        (roll-stat)
        (roll-stat)
        (roll-stat)
        (roll-stat)
        (roll-stat)))

;; <stats> class
(define-class <stats> ()
  [(str initform: 10)
   (dex initform: 10)
   (con initform: 10)
   (int initform: 10)
   (wis initform: 10)
   (cha initform: 10)])


;;; <stats> class constructors

;; all 10s
(define (make-stats)
  (make <stats>))

;; random values in each position
(define (random-stats)
  (make <stats> 'str (roll-stat)
                'dex (roll-stat)
                'con (roll-stat)
                'int (roll-stat)
                'wis (roll-stat)
                'cha (roll-stat)))

;; stat values prioritized according to order of order list
;(define (priority-stats . order)
;  (let ([nums (sort (stats-list) <)]
;        [unassigned (lset-difference *stat-symbols* order)])
;    (
    
