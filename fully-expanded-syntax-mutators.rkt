#lang racket
(provide swap*
         plain-lambda-swap*
         case-lambda-swap*
         if-swap*
         begin-swap*
         begin0-swap*
         let-values-swap*
         letrec-values-swap*
         with-continuation-mark-swap*
         plain-app-swap*)

(require "mutated.rkt"
         "mutator-lib.rkt"
         "mutate-expr.rkt"
         "mutate-program.rkt"
         "mutate-util.rkt"
         syntax/parse)

(define (atom? x)
  (and (not (list? x))
       (not (pair? x))))

(define (swap lst i j)
  (for/list ([e lst]
             [k (in-naturals 0)])
    (cond [(= k i) (list-ref lst j)]
          [(= k j) (list-ref lst i)]
          [else e])))

(define (find-index n k swap-dist)
  (cond
    [(>= swap-dist n) (values #f #f)]
    [(< k (- n swap-dist))
     (values k (+ k swap-dist))]
    [else
     (find-index n (- k (- n swap-dist)) (add1 swap-dist))]))

(define (num-max-swaps n)
  (/ (* n (- n 1)) 2))

(define (swap-kth lst k)
  (let ([n (length lst)])
    (cond
      [(>= k (num-max-swaps n)) #f]
      [else
       (let-values ([(i j) (find-index n k 1)])
         (swap lst i j))])))

; stx-list -> stx-list with kth swap
(define (swap-syntax-kth stx-list k accum-stx-list)
  (cond
    [(and (empty? stx-list) (negative? k))
     (reverse accum-stx-list)]
    [(and (empty? stx-list) (>= k 0)) #f]
    [(negative? k) (swap-syntax-kth (rest stx-list) k (cons (first stx-list) accum-stx-list))]
    [else
     (let ([stx (first stx-list)])
       (syntax-parse stx
         [(e ...)
          (let* ([stx-list2 (syntax->list #'(e ...))]
                 [num-possible-swaps (num-max-swaps (length stx-list2))])
            (if (>= k num-possible-swaps)
                (swap-syntax-kth (rest stx-list) (- k num-possible-swaps) (cons stx accum-stx-list))
                (swap-syntax-kth (rest stx-list) (- k num-possible-swaps)
                                 (cons #`(#,@(swap-kth stx-list2 k)) accum-stx-list))))]
         [(e ...+ . rest-id)
          (let* ([stx-list2 (append (syntax->list #'(e ...)) (list #'rest-id))]
                 [num-possible-swaps (num-max-swaps (length stx-list2))])
            (if (>= k num-possible-swaps)
                (swap-syntax-kth (rest stx-list) (- k num-possible-swaps) (cons stx accum-stx-list))
                (let ([swapped (swap-kth stx-list2 k)])
                  (let-values ([(swapped-formals swapped-rest-id)
                                (split-at swapped (sub1 (length swapped)))])
                    (swap-syntax-kth (rest stx-list) (- k num-possible-swaps)
                                     (cons #`(#,@swapped-formals . #,(first swapped-rest-id)) accum-stx-list))))))]
         [rest-id
          (swap-syntax-kth (rest stx-list) k (cons stx accum-stx-list))]))]))

(define (begin-swap stx counter)
  (syntax-parse stx
    #:literals (begin)
    [(begin expr ...+)
     (let ([swapped (swap-kth (syntax->list #'(expr ...)) counter)])
       (cond
         [(equal? swapped #f) #f]
         [else #`(begin #,@swapped)]))]
    [e #f]))

(define (begin0-swap stx counter)
  (syntax-parse stx
    #:literals (begin0)
    [(begin0 expr ...+)
     (let ([swapped (swap-kth (syntax->list #'(expr ...)) counter)])
       (cond
         [(equal? swapped #f) #f]
         [else #`(begin #,@swapped)]))]
    [e #f]))

(define (if-swap stx counter)
  (syntax-parse stx
    #:literals (if)
    [(if pred expr ...+)
     (let ([swapped (swap-kth (syntax->list #'(expr ...)) counter)])
       (cond
         [swapped #`(if pred #,@swapped)]
         [else #f]))]
    [e #f]))

(define (plain-lambda-formal-swap stx counter)
  (syntax-parse stx
    #:literals (#%plain-lambda)
    [(#%plain-lambda (formal ...) body ...+)
     (let ([swapped (swap-kth (syntax->list #'(formal ...)) counter)])
       (cond
         [swapped #`(#%plain-lambda #,swapped body ...)]
         [else #f]))]
    [(#%plain-lambda (formal ...+ . rest-id) body ...+)
     (let ([swapped (swap-kth (append (syntax->list #'(formal ...))
                                      (list #'rest-id))
                              counter)])
       (cond
         [swapped
          (let-values ([(swapped-formals swapped-rest-id)
                        (split-at swapped (sub1 (length swapped)))])
            #`(#%plain-lambda
               (#,@swapped-formals . #,(first swapped-rest-id)) body ...))]
         [else #f]))]
    [e #f]))


(define (plain-lambda-body-swap stx counter)
  (syntax-parse stx
    #:literals (#%plain-lambda)
    [(#%plain-lambda formals body ...+)
     (let ([swapped (swap-kth (syntax->list #'(body ...)) counter)])
       (cond
         [swapped #`(#%plain-lambda formals #,@swapped)]
         [else #f]))]
    [e #f]))


(define (case-lambda-case-swap stx counter)
  (syntax-parse stx
    #:literals (case-lambda)
    [(case-lambda case ...)
     (let ([swapped (swap-kth (syntax->list #'(case ...)) counter)])
       (cond
         [swapped #`(case-lambda #,@swapped)]
         [else #f]))]
    [e #f]))

(define (case-lambda-formals-swap stx counter)
  (syntax-parse stx
    #:literals (case-lambda)
    [(case-lambda [formals body ...+] ...)
     (let ([swapped-formals
            (swap-syntax-kth (syntax->list #'(formals ...)) counter empty)])
       (cond
         [swapped-formals
          #`(case-lambda #,@(map (λ (formal body) #`[#,formal #,@body])
                                 swapped-formals
                                 (syntax->list #'((body ...) ...))))]
         [else #f]))]
    [e #f]))

(define (case-lambda-body-swap stx counter)
  (syntax-parse stx
    #:literals (case-lambda)
    [(case-lambda [formals body ...+] ...)
     (let ([swapped-bodies
            (swap-syntax-kth (syntax->list #'((body ...) ...)) counter empty)])
       (cond
         [swapped-bodies
          #`(case-lambda #,@(map (λ (formal body) #`[#,formal #,@body])
                                 (syntax->list #'(formals ...))
                                 swapped-bodies))]
         [else #f]))]
    [e #f]))


(define (plain-app-swap stx counter)
  (syntax-parse stx
    #:literals (#%plain-app)
    [(#%plain-app proc-expr arg-expr ...)
     (let ([swapped (swap-kth (syntax->list #'(arg-expr ...)) counter)])
       (cond
         [swapped #`(#%plain-app proc-expr #,@swapped)]
         [else #f]))]
    [e #f]))

(define (let-values-id-swap stx counter)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values ([(id ...) val-expr] ...) body ...+)
     (let ([swapped-ids
            (swap-syntax-kth (syntax->list #'((id ...) ...)) counter empty)])
       (cond
         [swapped-ids
          #`(let-values
                (#,@(map (λ (swapped-ids val-e) #`[#,swapped-ids #,val-e])
                         swapped-ids
                         (syntax->list #'(val-expr ...))))
              body ...)]
         [else #f]))]
    [e #f]))

(define (let-values-binding-swap stx counter)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values (binding ...) body ...+)
     (let ([swapped-bindings
            (swap-kth (syntax->list #'(binding ...)) counter)])
       (cond
         [swapped-bindings
          #`(let-values #,swapped-bindings
              body ...)]
         [else #f]))]
    [e #f]))

(define (let-values-body-swap stx counter)
  (syntax-parse stx
    #:literals (let-values)
    [(let-values bindings body ...+)
     (let ([swapped-bodies
            (swap-kth (syntax->list #'(body ...)) counter)])
       (cond
         [swapped-bodies
          #`(let-values bindings #,@swapped-bodies)]
         [else #f]))]
    [e #f]))

(define (letrec-values-id-swap stx counter)
  (syntax-parse stx
    #:literals (letrec-values)
    [(letrec-values ([(id ...) val-expr] ...) body ...+)
     (let ([swapped-ids
            (swap-syntax-kth (syntax->list #'((id ...) ...)) counter empty)])
       (cond
         [swapped-ids
          #`(letrec-values
                (#,@(map (λ (swapped-ids val-e) #`[#,swapped-ids #,val-e])
                         swapped-ids
                         (syntax->list #'(val-expr ...))))
              body ...)]
         [else #f]))]
    [e #f]))

(define (letrec-values-binding-swap stx counter)
  (syntax-parse stx
    #:literals (letrec-values)
    [(letrec-values (binding ...) body ...+)
     (let ([swapped-bindings
            (swap-kth (syntax->list #'(binding ...)) counter)])
       (cond
         [swapped-bindings
          #`(letrec-values #,swapped-bindings
              body ...)]
         [else #f]))]
    [e #f]))

(define (letrec-values-body-swap stx counter)
  (syntax-parse stx
    #:literals (letrec-values)
    [(letrec-values bindings body ...+)
     (let ([swapped-bodies
            (swap-kth (syntax->list #'(body ...)) counter)])
       (cond
         [swapped-bodies
          #`(letrec-values bindings #,@swapped-bodies)]
         [else #f]))]
    [e #f]))

(define (with-continuation-mark-swap stx counter)
  (syntax-parse stx
    #:literals (with-continuation-mark)
    [(with-continuation-mark key-expr expr ...+)
     (let ([swapped (swap-kth (syntax->list #'(expr ...)) counter)])
       (cond
         [swapped #`(with-continuation-mark key-expr #,@swapped)]
         [else #f]))]
    [e #f]))



(define if-swap* (make-stream-mutator if-swap))

(define begin-swap* (make-stream-mutator begin-swap))

(define begin0-swap* (make-stream-mutator begin0-swap))

(define plain-app-swap* (make-stream-mutator plain-app-swap))

(define plain-lambda-swap*
  (apply compose-mutators
         (map make-stream-mutator
              (list plain-lambda-formal-swap
                    plain-lambda-body-swap))))

(define case-lambda-swap*
  (apply compose-mutators
         (map make-stream-mutator
              (list case-lambda-case-swap
                    case-lambda-formals-swap
                    case-lambda-body-swap))))

(define let-values-swap*
  (apply compose-mutators
         (map make-stream-mutator
              (list let-values-id-swap
                    let-values-binding-swap
                    let-values-body-swap))))

(define letrec-values-swap*
  (apply compose-mutators
         (map make-stream-mutator
              (list letrec-values-id-swap
                    letrec-values-binding-swap
                    letrec-values-body-swap))))

(define with-continuation-mark-swap*
  (make-stream-mutator with-continuation-mark-swap))

(define swap*
  (apply compose-mutators
         (list plain-lambda-swap*
               case-lambda-swap*
               if-swap*
               begin-swap*
               begin0-swap*
               let-values-swap*
               letrec-values-swap*
               with-continuation-mark-swap*
               plain-app-swap*)))

#|
(define mutate-expr (make-expr-mutator swap*))
(define mutate-program (make-program-mutator mutate-expr))
(define mutate-program-syntax (syntax-only mutate-program))

(define program-to-mutate #'{(require "a.rkt")
                             (define a (if (yes?) 0 42))
                             (define b (if (negative? a)
                                           "negative!"
                                           (if (zero? b)
                                               "zero!"
                                               "positive!")))
                             (define c (begin 1 2 3))
                             (define d (#%plain-lambda (x y z) x y z))
                             (define e (#%plain-lambda (x y . z) x y z))
                             (define f (case-lambda
                                         [() 10]
                                         [(x) x]
                                         [(x y) x y]
                                         [(x y . z) x y z]
                                         [r r]))
                             (define g (let-values ([(x y) (quotient/remainder 10 3)]
                                                    [(z w) (quotient/remainder 11 2)])
                                         (list y x)
                                         (list z w)))
                             (define h (letrec-values ([(is-even? is-odd?)
                                                        (values
                                                         (lambda (n)
                                                           (or (zero? n)
                                                               (is-odd? (sub1 n))))
                                                         (lambda (n)
                                                           (or (= n 1)
                                                               (is-even? (sub1 n)))))])
                                         (is-odd? 11)))
                             (define i (with-continuation-mark 1 2 3))
                             (define j (begin0 1 2 3))
                             (define k (#%plain-app + 1 2))
                             (displayln y)})
(with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
  (for ([i (in-naturals)])
    (displayln (list i (syntax->datum (mutate-program-syntax program-to-mutate i))))))
|#

#|

(define if-stx #'(if (begin 1 2) (begin 2 3) (begin 3 4)))
(define begin-stx #'(begin 1 2 3))
(define plain-lambda-stx #'(#%plain-lambda (x y z) x y z))
(define plain-lambda-stx-2 #'(#%plain-lambda (x y . z) x y z))
(define case-lambda-stx #'(case-lambda
                            [() 10]
                            [(x) x]
                            [(x y) x y]
                            [(x y . z) x y z]
                            [r r]))
(define let-values-stx #'(let-values ([(x y) (quotient/remainder 10 3)]
                                      [(z w) (quotient/remainder 11 2)])
                           (list y x)
                           (list z w)))

(define letrec-values-stx #'(letrec-values ([(is-even? is-odd?)
                                             (values
                                              (lambda (n)
                                                (or (zero? n)
                                                    (is-odd? (sub1 n))))
                                              (lambda (n)
                                                (or (= n 1)
                                                    (is-even? (sub1 n)))))])
                              (is-odd? 11)))
(define with-continuation-mark-stx #'(with-continuation-mark 1 2 3))

(define test (case-lambda-formals-swap case-lambda-stx 0))
;(case-lambda-body-swap case-lambda-stx 0)

#|
(let-values-id-swap let-values-stx 0)
(let-values-binding-swap let-values-stx 0)
(let-values-body-swap let-values-stx 0)
|#

(letrec-values-id-swap letrec-values-stx 0)
(letrec-values-binding-swap letrec-values-stx 0)
(letrec-values-body-swap letrec-values-stx 0)

(with-continuation-mark-swap with-continuation-mark-stx 0)
|#
#|
(define test (case-lambda-formals-swap case-lambda-stx 0))
;(plain-lambda-formal-rest-id-swap plain-lambda-stx-2 0)
(plain-lambda-formal-swap plain-lambda-stx 0)
(plain-lambda-formal-swap plain-lambda-stx-2 0)
(plain-lambda-formal-swap plain-lambda-stx-2 1)
(plain-lambda-body-swap plain-lambda-stx 0)
(plain-lambda-body-swap plain-lambda-stx-2 0)
|#

#|
(begin-swap-mutator begin-stx 0 0)
(if-swap-mutator if-stx 1 0)
(plain-lambda-formal-swap-mutator plain-lambda-stx 0 0)
(plain-lambda-body-swap-mutator plain-lambda-stx 0 0)
(plain-lambda-swap* plain-lambda-stx 0 0)
;(plain-lambda-formal-rest-id-swap-mutator plain-lambda-stx-2 0 0)



(define mutate-expr (make-expr-mutator begin-swap-mutator))
(define mutate-program (make-program-mutator mutate-expr))
(define mutate-program-syntax (syntax-only mutate-program))

(define program-to-mutate #'{(require "a.rkt")
                             (define x (if (yes?) 0 42))
                             (define y (if (negative? x)
                                           "negative!"
                                           (if (zero? x)
                                               "zero!"
                                               "positive!")))
                             (define z (begin 1 2 3))
                             (displayln y)})

(with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
  (for ([i (in-naturals)])
    (displayln (list i (syntax->datum (mutate-program-syntax program-to-mutate i))))))
|#

#|
(define-simple-mutator (if-swap stx)
  #:pattern ({~literal if} cond t e)
  #'(if cond e t))

(define-value-mutator constant-swap #:type "constant-swap" #:bind-value v
  [(? number?) #:-> (- v)])

(define active-mutators (list if-swap
                              constant-swap))

(define combined-active-mutators (apply compose-mutators active-mutators))
(define mutate-expr (make-expr-mutator combined-active-mutators))
(define mutate-program (make-program-mutator mutate-expr))
(define mutate-program-syntax (syntax-only mutate-program))

(define program-to-mutate #'{(require "a.rkt")
                             (define x (if (yes?) 0 42))
                             (define y (if (negative? x)
                                           "negative!"
                                           (if (zero? x)
                                               "zero!"
                                               "positive!")))
                             (displayln y)})
(with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
  (for ([i (in-naturals)])
    (displayln (list i (syntax->datum (mutate-program-syntax program-to-mutate i))))))
; =>
;; (0 ((require a.rkt) (define x (if (yes?) 42 0)) (define y (if (negative? x) negative! (if (zero? x) zero! positive!))) (displayln y)))
;; (1 ((require a.rkt) (define x (if (yes?) 0 -42)) (define y (if (negative? x) negative! (if (zero? x) zero! positive!))) (displayln y)))
;; (2 ((require a.rkt) (define x (if (yes?) 0 42)) (define y (if (negative? x) (if (zero? x) zero! positive!) negative!)) (displayln y)))
;; (3 ((require a.rkt) (define x (if (yes?) 0 42)) (define y (if (negative? x) negative! (if (zero? x) positive! zero!))) (displayln y)))
;; done!
|#