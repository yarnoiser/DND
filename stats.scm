(use miscmacros (srfi 1 95) coops coops-primitive-objects)

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
(define (priority-stats . order)
  (let ([nums (sort (stats-list) >)]
        [unassigned (lset-difference eqv? *stat-symbols* order)]
        [stats (make <stats>)])
    (for-each (lambda (stat)
                (set! (slot-value stats stat) (pop! nums)))
              order)
    (for-each (lambda (stat)
                (set! (slot-value stats stat) (pop! nums)))
              unassigned)
    stats))

;;; methods for stats class

;; swap values of two stats
(define-generic (stats-swap! stats stat1 stat2))

(define-method (stats-swap! (stats <stats>) (stat1 <symbol>) (stat2 <symbol>))
  (let ([tmp (slot-value stats stat1)])
    (set! (slot-value stats stat1) (slot-value stats stat2))
    (set! (slot-value stats stat2) tmp)))

(define-generic (inc-ability-score! stats stat))

(define-method (inc-ability-score! (stats <stats>) (stat <symbol>))
  (set! (slot-value stats stat) (add1 (slot-value stats stat))))


