#lang racket

#;(define/contract (get-length val)
    (-> list? integer?)
    (if (list? val) (length val) #f))

(define (get-length val)
  (if (list? val) (length val) #f))

(define (get-length-error val)
  (if (list? val) #f (length val)))

(module+ test
  (require rackunit)
  #;(require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-rackunit.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-defines.rkt"))
  (displayln "hello")
 
  (define val-not-ok (get-length-error 3))
  (define (check-even val)
    (if (equal? (remainder val 2) 0) #t #f))
  (define (add-1-to-length val)
    (+ 1 (get-length val)))
  (define-values (v1 v2) (values (error "hi") 2))
  (check-equal? val-not-ok 3)
  (check-true (check-even 2))
  (check-equal? (add-1-to-length 3) 3)
  
  (check-eq? (list 1) (list 1) "hello")
  (check-eq? 3 3)
  (check-eq? (length 3) 4 "hi")

  (check-not-eq? 1 1 "fixnums are eq?")
  (check-not-eq? 1 2 "fixnums are eq?")
  (check-not-eq? 1 (length 3) "fixnums are eq?")

  (check-eqv? 1 1.0 "not eqv?")
  (check-eqv? 1 1 "not eqv?")
  (check-eqv? (length 3) 1.0 "not eqv?")

  (check-not-eqv? 1 1 "integers are eqv?")
  (check-not-eqv? 1 1.0 "integers are eqv?")
  (check-not-eqv? 1 (length 3) "integers are eqv?")

  (check-equal? 1 1.0 "not equal?")
  (check-equal? 3 3)
  (check-equal? (length 3) 4 "hi")

  (check-not-equal? (list 1) (list 1) "equal?")
  (check-not-equal? (list 1) (list 2) "equal?")
  (check-not-equal? (list 1) (length 3) "equal?")

  (check-pred number? "I fail")
  (check-pred string? "I work")
  (check-pred number? (length 3))

  (check-= 1.0 1.01 0.005 "I fail")
  (check-= 1.0 1.01 0.02 "I work")
  (check-= 1.0 (length 1) 0.005 "I fail")

  (check-within (list 6e+23 10.0) (list 6.02e+23 9.8) 0.05)
  (check-within (list 6 10) (list 6.02 9.99) 0.05)
  (check-within (length 3) (list 6.02e+23 9.8) 0.05)

  (check-true #t)
  (check-true #f)
  (check-true (length 3))

  (check-exn exn:fail?
             (lambda ()
               (break-thread (current-thread))))
  (check-exn
   exn:fail?
   (lambda ()
     (error 'hi "there")))
  (check-exn exn:fail?
             (error 'hi "there"))

  (check-not-exn (λ () 1))
  (check-not-exn (λ () (car '())))
  (check-not-exn (λ () (/ 1 0)) "don't divide by 0")
  (check-not-exn (error 'hi "there"))

  (check-regexp-match "a+bba" "aaaaaabba")
  (check-regexp-match "a+bba" "aaaabbba")
  (check-regexp-match (length 3) "aaaabbba")
  (check-regexp-match 3 "aaaabbba")

  (check-match (list 1 2 3) (list _ _ 4))
  (check-match (list 1 2 3) (list _ _ 3))
  (check-match (list 1 (list 3)) (list x (list _)) (odd? x))
  (check-match 6 x (odd? x))
  (check-match (list 1 2) (list x) (odd? x))
  (check-match (length 3) (list _ _ 3))

  (check < 2 3)
  (check memq 'pine '(apple orange pear))
  (check < 2 (length 3))

  (fail (length 3))
  #|
  (require rackunit rackunit/text-ui)
  (define val-ok-if-not-mutated (get-length 3))
  (define (check-even val)
    (if (equal? (remainder val 2) 0) #t #f))
  (define (another-fn x y) (+ x x y))
  (parameterize* ([current-check-around (λ (chk-thunk)
                                          (displayln "hi")
                                          (chk-thunk))]
                  #;[test-suite-check-around (λ (chk-thunk)
                                               (displayln "hi2")
                                               (chk-thunk))])
    (check-false val-ok-if-not-mutated)
    (check-equal? (get-length '(1 2 3)) 3)
    (check-true (check-even 2))
    (run-tests (test-suite "test-suite-check-around-test"
                           (check-equal? (get-length '(1 2 3)) 3)
                           (check-false (check-even 3)))
               'verbose))
|#

  )

; scenarios
; 1. value defined in test causes an error due to mutation in the program
; e.g. (define test-val (some-fn 3))
; some-fn is mutated to cause an error

; 2. function defined in test causes an error due to calling some mutated function of the program
; e.g. nothing to do because by default lazy