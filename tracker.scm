(define-syntax heal! (syntax-rules ()
  [(_ var val)
   (begin (set! var (+ var val))
          var)]))

(define-syntax damage! (syntax-rules ()
  [(_ var val)
   (begin (set! var (- var val))
          var)]))

(define-syntax dmg! (syntax-rules ()
  [(_ var val)
   (damage! var val)]))

(define-syntax d! (syntax-rules ()
  [(_ var val)
   (damage! var val)]))

