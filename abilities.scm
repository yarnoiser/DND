(use miscmacros (srfi 1 95) coops coops-primitive-objects)

(define *ability-symbols* '(str dex con int wis cha))

(define (ability-bonus ability)
  (inexact->exact (floor (/ (- ability 10) 2))))

(define (ability-roll)
  (let*
    ([lst (list (d6) (d6) (d6) (d6))]
     [lowest (apply min lst)])
       (let-values ([(low high) (partition (lambda (x) (= x lowest)) lst)])
         (if (< 1 (length low))
           (apply + (append high (cdr low)))
           (apply + high)))))

;; list of starting ability numbers
(define (abilities-list)
  (list (ability-roll)
        (ability-roll)
        (ability-roll)
        (ability-roll)
        (ability-roll)
        (ability-roll)))

;; <abilities> class
(define-class <abilities> ()
  [(str initform: 10)
   (dex initform: 10)
   (con initform: 10)
   (int initform: 10)
   (wis initform: 10)
   (cha initform: 10)])


;;; <abilitiess> class constructors

;; all 10s
(define (make-abilities)
  (make <abilities>))

;; random values in each position
(define (random-abilities)
  (make <abilities> 'str (ability-roll)
                    'dex (ability-roll)
                    'con (ability-roll)
                    'int (ability-roll)
                    'wis (ability-roll)
                    'cha (ability-roll)))

;; ability values prioritized according to order of order list
(define (priority-abilities . order)
  (let ([nums (sort (abilities-list) >)]
        [unassigned (lset-difference eqv? *ability-symbols* order)]
        [stats (make <abilities>)])
    (for-each (lambda (stat)
                (set! (slot-value abilities ability) (pop! nums)))
              order)
    (for-each (lambda (stat)
                (set! (slot-value abilities ability) (pop! nums)))
              unassigned)
    ability))

;;; methods for stats class

;; swap values of two stats
(define-generic (abilities-swap! abilities stat1 stat2))

(define-method (abilities-swap! (abilities <abilities>) (ability1 <symbol>) (ability2 <symbol>))
  (let ([tmp (slot-value abilities ability1)])
    (set! (slot-value abilities ability1) (slot-value abilities ability2))
    (set! (slot-value abilities ability2) tmp)))

(define-generic (inc-ability-score! abilities ability))

(define-method (inc-ability-score! (abilities <abilities>) (ability <symbol>))
  (set! (slot-value abilities ability) (add1 (slot-value abilities ability))))

