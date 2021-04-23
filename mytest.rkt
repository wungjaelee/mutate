#lang racket

#;(define/contract (get-length val)
    (-> list? integer?)
    (if (list? val) (length val) #f))

(define (get-length val)
  (if (list? val) (length val) #f))

(module+ test
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
               'verbose)))

; scenarios
; 1. value defined in test causes an error due to mutation in the program
; e.g. (define test-val (some-fn 3))
; some-fn is mutated to cause an error

; 2. function defined in test causes an error due to calling some mutated function of the program
; e.g. nothing to do because by default lazy