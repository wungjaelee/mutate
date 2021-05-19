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
  (define stx (extract-syntax path))
  (define test-suite-transformed-stx (transform-test-suite stx))
  (log-test-info (pretty-format (syntax->datum test-suite-transformed-stx)))
  (define fully-expanded-stx (expand-and-disarm test-suite-transformed-stx #:directory dir))
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
        (with-handlers ([exn? (λ (e) (log-test-info "top level exception ~a\n" e))])
          (eval-syntax mutant))))
    ))

#;(define (make-instrument-module test-module target-module-to-mutate mutate-index)
  (λ (path-string _)
    (cond
      [(equal? path-string test-module) (mutate-rackunit-tests path-string)]
      [(equal? path-string target-module-to-mutate) ])))

(when (= (vector-length (current-command-line-arguments)) 1)
  (let ([module-path (vector-ref (current-command-line-arguments) 0)])
    (run-experiment (path->complete-path (string->path module-path))
                    if-swap*)))
