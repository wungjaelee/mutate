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

(define mini-swap*
  (apply compose-mutators
         (list plain-lambda-swap*
               if-swap*)))

(define (attach-exn-handler-to-top-level-exprs test-module-stx)
  (syntax-parse test-module-stx
    [({~datum require} program)
     (datum->syntax test-module-stx
                    `(require ,#'program)
                    test-module-stx
                    test-module-stx)]
    [({~datum define} (fn-id args ...) body0 body ...)
     (datum->syntax test-module-stx
                    `(define (,#'fn-id ,@#'(args ...)) ,#'body0 ,@#'(body ...))
                    test-module-stx
                    test-module-stx)]
    [({~datum define} id body)
     (datum->syntax test-module-stx
                    `(define ,#'id
                       (with-handlers
                           ([exn?
                             (λ (e)
                               (log-test-info "DEFINE-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised " ',#'id e)
                               'dummy-val)])
                         ,#'body))
                    test-module-stx
                    test-module-stx)]
    [({~datum define-values} (id ...) body)
     (datum->syntax test-module-stx
                    `(define-values ,#'(id ...)
                       (with-handlers
                           ([exn?
                             (λ (e) (log-test-info "DEFINE-VALUES-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised " ',#'(id ...) e)
                               (values ,@(map (λ (id) ''dummy-val) (syntax->list #'(id ...)))))])
                         ,#'body))
                    test-module-stx
                    test-module-stx)]
    [expr
     (datum->syntax test-module-stx
                    `(with-handlers
                         ([exn?
                           #;(λ (exn) (log-test-info "EXCEPTION: Test ~a ~a raised an exception ~a" test-id ',#'expr exn))
                           (λ (exn) (log-test-info "EXCEPTION: Test ~a raised an exception ~a" test-id exn))])
                       (set! test-id (add1 test-id))
                       ,#'expr)
                    test-module-stx
                    test-module-stx)]))

(define (transform-test-suite program-stx)
  (syntax-parse program-stx
    [({~datum module+} {~datum test} {~and the-require ({~datum require} prgm ...)} ... body ...)
     (datum->syntax program-stx
                    `(module+ test
                       ,@(map transform-test-suite (syntax->list #'(the-require ...)))
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt")
                                (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-defines.rkt"))
                       ,@#'(body ...))
                    program-stx
                    program-stx)]
    [{~datum rackunit}
     (datum->syntax program-stx
                    `(file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-rackunit.rkt")
                    program-stx
                    program-stx)]
    [(e ...)
     (datum->syntax
      program-stx
      (map transform-test-suite (syntax->list #'(e ...)))
      program-stx
      program-stx)]
    [e
     (datum->syntax program-stx `,#'e program-stx program-stx)]))



(define (mutate-rackunit-tests path)
  (define-values (dir file is-dir) (split-path path))
  (define ns (make-base-namespace))
  (namespace-attach-module (current-namespace) "my-logger.rkt" ns)
  (define stx (extract-syntax path ns))
  ;(display stx)
  (define test-suite-transformed-stx (transform-test-suite stx))
  (display test-suite-transformed-stx)
  (log-test-info (pretty-format (syntax->datum test-suite-transformed-stx)))
  (define fully-expanded-stx (expand-and-disarm test-suite-transformed-stx ns #:directory dir))
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
  (with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
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
        #;(eval-syntax mutant)
        (with-handlers ([exn? (λ (e) (log-test-info "top level exception ~a\n" e))])
          (eval-syntax mutant))))
    ))

; 1. syntax error at (define mutant ...) => wrap it with with-handlers 
; 2. dynamic error if error at (eval-syntax mutant)
; 3. just wrap every top level expressions with with-handlers, with the exception of define
; 4. go to package server, select few packages with different authors (e.g. gregor date time library)

;(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/board.rkt" if-swap*)

#;(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/mytest.rkt" if-swap*)

#;(run-experiment "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/experiments/santorini/player.rkt" swap*)

(when (= (vector-length (current-command-line-arguments)) 1)
    (run-experiment (vector-ref (current-command-line-arguments) 0)
                    if-swap*))


#;(mutate-rackunit-tests "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/test-my-rackunit.rkt")

#;(mutate-rackunit-tests "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/gregor/gregor-lib/gregor/private/clock.rkt")