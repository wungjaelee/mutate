#lang racket

(require (prefix-in @ rackunit)
         rackunit/text-ui
         syntax/parse/define
         (for-syntax racket/syntax)
         (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt"))

;(define log-test-info printf)

; basic checks

#;(define-syntax-parser check-equal?
    [(check-equal? v1 v2 {~optional message})
     #'(parameterize ([@current-check-around
                       (λ (chk-thunk)
                         (with-handlers ([@exn:test:check? (λ (e) (log-test-info "FAIL: test failed"))])
                           (chk-thunk)
                           (log-test-info "PASS: test passed")))])
         (with-handlers ([exn? (λ (e) (log-test-info "EXCEPTION: test raised an error ~a" e))])
           (@check-equal? v1 v2)))])

(define-syntax-parser make-basic-check
  [(make-basic-check check-name)
   (with-syntax ([original-rackunit-check
                  (format-id #'check-name "@~a" #'check-name)])
     #'(define-syntax-parser check-name
         [(check-name args (... ...))
          #'(parameterize ([@current-check-around
                            (λ (chk-thunk)
                              (with-handlers ([@exn:test:check? (λ (e) (log-test-info "FAIL: test failed"))]
                                              [exn? (λ (e) (log-test-info "ERROR-FAIL: test failed with error ~a" e))])
                                (chk-thunk)
                                (log-test-info "PASS: test passed")))])
              (with-handlers ([exn? (λ (e) (log-test-info "ERROR: test raised an error ~a" e))])
                (log-test-info "Running test ~a" (cons 'check-name (map (λ (stx) (syntax->datum stx)) (syntax->list #'(args (... ...))))))
                (original-rackunit-check args (... ...))))]))])

(make-basic-check check-eq?)
(make-basic-check check-not-eq?)
(make-basic-check check-eqv?)
(make-basic-check check-not-eqv?)
(make-basic-check check-equal?)
(make-basic-check check-not-equal?)
(make-basic-check check-pred)
(make-basic-check check-=)
(make-basic-check check-within)
(make-basic-check check-true)
(make-basic-check check-false)
(make-basic-check check-not-false)
(make-basic-check check-exn)
(make-basic-check check-not-exn)
(make-basic-check check-regexp-match)
(make-basic-check check-match)
(make-basic-check check)
(make-basic-check fail)


#;(define-syntax-parser define-simple-check
  [(define-simple-check (name param ...) body ...)
   (with-syntax 
   #'(begin
       (@define-simple-check (@name param ...) body ...)
       (make-basic-check name)))])

#;(define-simple-check (check-odd blah)) ;; this is inside
#;(make-basic-check check-odd)


#;(define-syntax-parser run-tests)
  

(provide check-eq?
         check-not-eq?
         check-eqv?
         check-not-eqv?
         check-equal?
         check-not-equal?
         check-pred
         check-=
         check-within
         check-true
         check-false
         check-not-false
         check-exn
         check-not-exn
         check-regexp-match
         check-match
         check
         fail
         run-tests
         (rename-out [@test-suite test-suite]
                     [@test-case test-case]))



   
   

#|
Questions

1. remove paren from variable number of arguments
2. define-simple-check
3. error containing define identifier, make it to dummy-val so that test fail? or define it to error so that it will raise an error?
|#
