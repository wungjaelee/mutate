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

#;(require syntax/parse
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/fully-expand.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/fully-expanded-syntax-mutators.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mutated.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mutator-lib.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mutate-expr.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mutate-program.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mutate-util.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/top-level-selectors.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/rackunit-test-mutators.rkt")
           (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt")
           rackunit)

(define swap*
  (apply compose-mutators
         (list plain-lambda-swap*
               if-swap*)))

(define mutate-expr (make-expr-mutator if-swap*))
(define mutate-program (make-program-mutator mutate-expr
                                             select-except-test-module))
(define mutate-program-syntax (syntax-only mutate-program))

;; for wrapping rackunit test functions with input logging
(define mutate-rackunit-expr (make-expr-mutator wrap-check-equal))
(define mutate-rackunit-program (make-program-mutator mutate-rackunit-expr))
(define mutate-rackunit-program-syntax (syntax-only mutate-rackunit-program))

(define (mutate-all module-stx program-mutator)
  (define mutant module-stx)
  (with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
    (for ([i (in-naturals)])
      (set! mutant (program-mutator mutant i))))
  mutant)


;(define test-mutate-namespace (make-base-namespace))
#;(define tp (extract-syntax "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/test2.rkt" test-mutate-namespace))
;(define mtp (mutate-test-module-syntax tp 0))
;(displayln "hi")
;(define mtp (mutate-all tp))
;(displayln "hi2")
;(define fmtp (expand-and-disarm mtp test-mutate-namespace))

(define (my-mutate program-stx [namespace (current-namespace)])
  (parameterize ([current-namespace namespace])
    (syntax-parse program-stx
      #;[({~datum module} name racket body ...)
         #'(module name racket
             body ...)]
      [({~datum module+} test something body ...)
       (datum->syntax program-stx
                      `(module+ test ,#'something (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt")) ,@(map my-mutate (syntax->list #'(body ...)))))]
      ;(module test racket
      ;#,(map my-mutate (syntax->list #'(body ...))))]
      [({~datum check-equal?} e1 e2)
       (datum->syntax
        program-stx
        `(begin (log-test-info "Running test (check-equal? ~a ~a)\n" ,#'e1 ,#'e2)
                (check-equal? ,#'e1 ,#'e2))
        program-stx
        program-stx)]
      [(e ...)
       #`(#,@(map my-mutate (syntax->list #'(e ...))))]
      [e (datum->syntax program-stx `,#'e program-stx program-stx)])))

(define hi (extract-syntax "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/test2.rkt"))

#;(my-mutate (list-ref (syntax->list (list-ref (syntax->list (list-ref (syntax->list hi) 3)) 5)) 3))

(define (mutate-rackunit-tests path)
  (define-values (dir file is-dir) (split-path path))
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) "my-logger.rkt" ns)
  (define stx (extract-syntax path ns))
  ;(define rackunit-mutated-stx (mutate-all stx mutate-rackunit-program-syntax))
  (define rackunit-mutated-stx (my-mutate stx ns))
  ;(define rackunit-mutated-stx (mutate-rackunit-program-syntax stx 0))
  (displayln rackunit-mutated-stx)
  (define fully-expanded-stx (expand-and-disarm rackunit-mutated-stx ns))
  fully-expanded-stx)

(define (make-mutant my-module-stx i)
  (syntax-parse my-module-stx
    [(module name lang (#%module-begin top-level-form ...))
     #:with [mutated-top-level-form ...] (mutate-program-syntax #'[top-level-form ...] i)
     #'(begin (module name lang (#%module-begin mutated-top-level-form ...))
              (require (submod 'name test)))]))

(define (add-begin-require my-module-stx)
  (syntax-parse my-module-stx
    [(module name lang body ...)
     #'(begin (module name lang body ...)
              (require (submod 'name test)))]))

#;(define target-program
    (extract-fully-expanded-program "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/board.rkt"))

(define target-program
  (mutate-rackunit-tests "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/test2.rkt"))


; (eval-syntax (add-begin-require target-program))

#;(define-runtime-module-path-index board.rkt-mpi "experiments/santorini/board.rkt")

;current-require-module-path
;current-directory

(with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
    (for ([i (in-naturals)])
      (define mutant (make-mutant target-program i))
      (log-test-info "mutant ~a" i)
      ;(log-test-info (pretty-format (syntax->datum mutant)))
      (define this-ns (current-namespace))
      (define test-id 0)
      ;(define my-check-around (make-parameter default-check-around))
      (parameterize ([current-namespace (make-base-namespace)]
                     #;[current-require-module-path board.rkt-mpi])
        (namespace-attach-module this-ns 'rackunit)
        (parameterize ([current-check-around
                        (λ (chk-thunk)
                          (with-handlers ([exn:test:check? (λ (e) (log-test-info "test ~a failed" test-id))]
                                          [exn:fail? (λ (e) (log-test-info "test ~a raised an error ~a" test-id e))])
                            (set! test-id (add1 test-id))
                            (chk-thunk)
                            (log-test-info "test ~a passed" test-id)))])
          (with-handlers ([exn:fail? (λ (e) (log-test-info "test ~a raised an error ~a" test-id e))])
            (eval-syntax mutant))))))


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