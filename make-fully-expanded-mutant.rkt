#lang racket
(provide make-make-instrument-module/cached)

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
    [({~datum module} name {~datum racket} ({~datum #%module-begin} {~and the-require ({~datum require} prgm ...)} ... body ...))
     (datum->syntax program-stx
                    `(module ,#'name racket
                       (#%module-begin
                       ,@(map transform-test-suite (syntax->list #'(the-require ...)))
                       (require (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-logger.rkt")
                                (file "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/my-defines.rkt"))
                       ,@#'(body ...)))
                    program-stx
                    program-stx)]
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

(define (fully-expand path)
  (define-values (dir file is-dir) (split-path path))
  (define stx (extract-syntax path))
  (define fully-expanded-stx (expand-and-disarm stx #:directory dir))
  fully-expanded-stx)

(define (make-mutant my-module-stx program-mutator i [add-begin-require #f])
  (syntax-parse my-module-stx
    [(module name lang (#%module-begin top-level-form ...))
     #:with [mutated-top-level-form ...] (program-mutator #'[top-level-form ...] i)
     (if add-begin-require
         #'(begin (module name lang (#%module-begin mutated-top-level-form ...))
                  (require (submod 'name test)))
         #'(module name lang (#%module-begin mutated-top-level-form ...)))]))

(define (add-begin-require my-module-stx)
  (syntax-parse my-module-stx
    [(module name lang body ...)
     #'(begin (module name lang body ...)
              (require (submod 'name test)))]))

(define (make-stx-only-program-mutator basic-mutator)
  (define expr-mutator (make-expr-mutator basic-mutator))
  (define program-mutator (make-program-mutator expr-mutator
                                                select-except-test-module))
  (syntax-only program-mutator))

(define (run-experiment program-file-path mutator)
  ; set up program by wrapping all rackunit tests
  (define target-program (mutate-rackunit-tests program-file-path))

  ; make program mutator
  (define stx-only-program-mutator (make-stx-only-program-mutator mutator))

  ; generate all mutants and run the tests
  (define-values (dir file is-dir) (split-path program-file-path))
  (with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
    (for ([i (in-naturals)])
      (define mutant (make-mutant target-program stx-only-program-mutator i #t))
      (log-test-info "mutant ~a" i)
      ;(log-test-info (pretty-format (syntax->datum mutant)))
      (parameterize ([current-namespace (make-base-namespace)])
        (with-handlers ([exn? (λ (e) (log-test-info "top level exception ~a\n" e))])
          (eval-syntax mutant))))
    ))

(define (make-make-instrument-module/cached test-module target-module-to-mutate mutator)
  (let ([cache (make-hash)]
        [program-mutator (make-stx-only-program-mutator mutator)])
    (λ (mutate-index)
      (λ (path-string _)
        (log-test-info "expanding ~a\n" path-string)
        (cond
          [(equal? path-string test-module)
           (hash-ref! cache path-string (thunk (mutate-rackunit-tests path-string)))]
          [(and (equal? mutate-index #f) (equal? path-string target-module-to-mutate))
           (log-test-info "RUNNING SANITY CHECK: ALL TESTS MUST PASS\n")
           (hash-ref! cache path-string (thunk (fully-expand target-module-to-mutate)))]
          [(equal? path-string target-module-to-mutate)
           (define target-module-stx
             (hash-ref! cache path-string (thunk (fully-expand target-module-to-mutate))))
           (make-mutant target-module-stx program-mutator mutate-index)]
          [else
           (hash-ref! cache path-string (thunk (fully-expand path-string)))])))))

(when (= (vector-length (current-command-line-arguments)) 1)
  (let ([module-path (vector-ref (current-command-line-arguments) 0)])
    (run-experiment (path->complete-path (string->path module-path))
                    if-swap*)))
