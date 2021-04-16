#lang racket
(require syntax/parse
         "fully-expand.rkt"
         "fully-expanded-syntax-mutators.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "mutate-expr.rkt"
         "mutate-program.rkt"
         "mutate-util.rkt"
         "top-level-selectors.rkt"
         "rackunit-test-mutators.rkt"
         "my-logger.rkt"
         rackunit
         racket/require-transform
         racket/runtime-path)

(define swap*
  (apply compose-mutators
         (list plain-lambda-swap*
               if-swap*)))

(define mutate-expr (make-expr-mutator if-swap*))
(define mutate-program (make-program-mutator mutate-expr
                                             select-except-test-module))
(define mutate-program-syntax (syntax-only mutate-program))


(define (wrap-rackunit-tests program-stx)
  (syntax-parse program-stx
    [({~datum module+} {~datum test} something body ...)
     (datum->syntax program-stx
                    `(module+ test ,#'something
                       (define test-id 0)
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt"))
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/test-mutators.rkt"))
                       (parameterize ([current-check-around
                                       (λ (chk-thunk)
                                         (with-handlers ([exn:test:check? (λ (e) (log-test-info "test ~a failed" test-id))])
                                           (chk-thunk)
                                           (log-test-info "test ~a passed" test-id)))])
                         ,@(map wrap-rackunit-tests (syntax->list #'(body ...))) ))
                    program-stx
                    program-stx)]
    [({~and the-check {~or {~datum check-eq?}
                           {~datum check-not-eq?}
                           {~datum check-eqv?}
                           {~datum check-not-eqv?}
                           {~datum check-equal?}
                           {~datum check-not-equal?}
                           {~datum check-pred}
                           {~datum check-=}
                           {~datum check-within}
                           {~datum check-true}
                           {~datum check-false}
                           {~datum check-not-false}
                           {~datum check-exn}
                           {~datum check-not-exn}
                           {~datum check-regexp-match}
                           {~datum check-match}}}
      exprs ...)
     (let ([expr-datums (map syntax->datum (syntax->list #'(exprs ...)))]
           [the-check-datum (syntax->datum #'the-check)])
       (datum->syntax program-stx
                      `(begin
                         (set! test-id (add1 test-id))
                         (log-test-info "Running test ~a (~a ~s)" test-id ',the-check-datum ',expr-datums)
                         (with-handlers ([exn?
                                          (λ (e)
                                            (log-test-info "Test ~a raised an error with exception ~a" test-id e))])
                           (,the-check-datum ,@#'(exprs ...))))
                      program-stx
                      program-stx))]
    [(e ...)
     (datum->syntax
      program-stx
      (map wrap-rackunit-tests (syntax->list #'(e ...)))
      program-stx
      program-stx)]
    [e
     (datum->syntax program-stx `,#'e program-stx program-stx)]))


#;(define (wrap-rackunit-tests program-stx)
  (syntax-parse program-stx
    [({~datum module+} {~datum test} something body ...)
     (datum->syntax program-stx
                    `(module+ test ,#'something
                       (define test-id 0)
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt"))
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/test-mutators.rkt"))
                       (parameterize ([current-check-around
                                       (λ (chk-thunk)
                                         (with-handlers ([exn:test:check? (λ (e) (log-test-info "test ~a failed" test-id))])
                                           (chk-thunk)
                                           (log-test-info "test ~a passed" test-id)))])
                         ,@(map wrap-rackunit-tests (syntax->list #'(body ...))) ))
                    program-stx
                    program-stx)]
    [({~datum check-equal?} e1 e2)
     (define val1 (syntax->datum #'e1))
     (define val2 (syntax->datum #'e2))
     (datum->syntax
      program-stx
      `(begin
         (set! test-id (add1 test-id))
         (log-test-info "Running test ~a (check-equal? ~a ~a)" test-id ',val1 ',val2)
         (with-handlers ([exn? (λ (e) (log-test-info "Test ~a raised an error with exception ~a" test-id e))])
           (check-equal? ,#'e1 ,#'e2)))
      program-stx
      program-stx)]
    [({~datum check-true} e1)
     (define val1 (syntax->datum #'e1))
     (datum->syntax
      program-stx
      `(begin
         (set! test-id (add1 test-id))
         (log-test-info "Running test ~a (check-true ~a ~a)" test-id ',val1)
         (with-handlers ([exn? (λ (e) (log-test-info "Test ~a raised an error with exception ~a" test-id e))])
           (check-true? ,#'e1)))
      program-stx
      program-stx)]
    [({~datum check-false} e1)
     (define val1 (syntax->datum #'e1))
     (datum->syntax
      program-stx
      `(begin
         (set! test-id (add1 test-id))
         (log-test-info "Running test ~a (check-false ~a ~a)" test-id ',val1)
         (with-handlers ([exn? (λ (e) (log-test-info "Test ~a raised an error with exception ~a" test-id e))])
           (check-true? ,#'e1)))
      program-stx
      program-stx)]
    [(e ...)
     (datum->syntax
      program-stx
      (map wrap-rackunit-tests (syntax->list #'(e ...)))
      program-stx
      program-stx)]
    [e
     (datum->syntax program-stx `,#'e program-stx program-stx)]))


(define (mutate-rackunit-tests path)
  (define-values (dir file is-dir) (split-path path))
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) "my-logger.rkt" ns)
  (define stx (extract-syntax path ns))
  (define rackunit-mutated-stx (wrap-rackunit-tests stx))
  (log-test-info (pretty-format (syntax->datum rackunit-mutated-stx)))
  (define fully-expanded-stx (expand-and-disarm rackunit-mutated-stx ns #:directory dir))
  fully-expanded-stx)

(define (make-mutant my-module-stx program-mutator i)
  (syntax-parse my-module-stx
    [(module name lang (#%module-begin top-level-form ...))
     #:with [mutated-top-level-form ...] (program-mutator #'[top-level-form ...] i)
     #'(begin (module name lang (#%module-begin mutated-top-level-form ...))
              (require (submod 'name test)))]))

(define (add-begin-require my-module-stx)
  (syntax-parse my-module-stx
    [(module name lang body ...)
     #'(begin (module name lang body ...)
              (require (submod 'name test)))]))


;(define target-program (mutate-rackunit-tests "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/board.rkt"))

(define (run-experiment program-file-path mutator)
  ; set up program by wrapping all rackunit tests
  (define target-program (mutate-rackunit-tests program-file-path))

  ; make program mutator
  (define expr-mutator (make-expr-mutator mutator))
  (define program-mutator (make-program-mutator expr-mutator
                                                select-except-test-module))
  (define stx-only-program-mutator (syntax-only program-mutator))

  ; generate all mutants and run the tests
  (define-values (dir file is-dir) (split-path program-file-path))
  (with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))]
                  [exn? (λ (e) (printf "top handler ~a\n" e))])
    (for ([i (in-naturals)])
      (define mutant (make-mutant target-program stx-only-program-mutator i))
      (log-test-info "mutant ~a" i)
      ;(log-test-info (pretty-format (syntax->datum mutant)))
      (define this-ns (current-namespace))
      (define test-id 0)
      (parameterize ([current-namespace (make-base-namespace)]
                     [current-directory dir])
        (namespace-attach-module this-ns 'rackunit)
        (namespace-attach-module this-ns 'racket)
        (eval-syntax mutant))))
  )

;(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/board.rkt" if-swap*)

;(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/mytest.rkt" if-swap*)

(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/player.rkt" if-swap*)




#|
1. Goal: test logging
    => write macro that prints test and then run test
    => work on the level of program syntax, not fully expanded one
    => how to avoid repeating?
2. Goal: exception raised for tests due to wrong transformation
|#




#|
Questions
1. How to read the program, not as fully expanded syntax, but as raw?


1. Make a constructor for the namespace
  - add rackunit to the namespace
  - parameterize on eval-syntax
  - write my own macro that wraps eval-syntax

Information
1. current mutation
2. what error we got
3. whether a test succeeded
4. log the test case
5. log the inputs to the test case
6. log the mutated program

for each file
=> file has 70 tests
=> 56/70 test failed for this mutation
=> 10/70 test passed
=> 4/70 raised an error before running the test
|#

; edge case
; user-defined test functions e.g. check-permutation
; problem:
; 1. how do we know that it's a user-defined test function?
; 2. if it boils down to rackunit tests
; 3. cannot catch at rackunit test level because it's a function call and the arguments get evaluated first when it is passed in which then possibly throws an error. => function inlining can avoid this problem. Or crude but work is to wrap define with with-handler
; 