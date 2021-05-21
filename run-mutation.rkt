#lang racket
(require "instrument-module.rkt"
         "make-fully-expanded-mutant.rkt"
         "mutator-lib.rkt"
         "mutate-program.rkt"
         "my-logger.rkt"
         "fully-expanded-syntax-mutators.rkt")

(define (run-mutation test-module module-to-mutate other-modules mutator)
  (define make-instrument-module/cached
    (make-make-instrument-module/cached test-module module-to-mutate mutator))
  (define sanity-run
    (make-instrumented-module-runner test-module
                                     other-modules
                                     (make-instrument-module/cached #f)))
  ; sanity check, all test must pass
  (sanity-run)

  ; running mutants
  (with-handlers ([mutation-index-exception? (λ _ (displayln 'done!))])
    (for ([i (in-naturals)])
      (define instrument-module (make-instrument-module/cached i))
      (define run
        (make-instrumented-module-runner test-module
                                         other-modules
                                         instrument-module))
      ;(log-test-info (pretty-format (syntax->datum mutant)))
      (parameterize ([current-namespace (make-base-namespace)])
        (log-test-info "mutant ~a" i)
        (with-handlers ([exn? (λ (e) (log-test-info "top level exception ~a\n" e))])
          (run))))))

(define (build-module-paths base-dir
                            lib-subdir
                            test-subdir
                            test-module-filename
                            module-to-mutate-filename
                            other-module-filenames)
  (values (build-path base-dir test-subdir test-module-filename)
          (build-path base-dir lib-subdir module-to-mutate-filename)
          (map (λ (filename) (build-path base-dir lib-subdir filename))
               (cons module-to-mutate-filename other-module-filenames))))

#;(let-values ([(test-module module-to-mutate other-modules)
                (build-module-paths
                 "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/gregor/"
                 "gregor-lib/gregor/private/"
                 "gregor-test/gregor/tests/"
                 "clock.rkt"
                 "clock.rkt"
                 (list "moment.rkt"
                       "date.rkt"
                       "datetime.rkt"
                       "time.rkt"
                       "iso8601-parse.rkt"))])
    (run-mutation test-module module-to-mutate other-modules if-swap*))


(let-values ([(test-module module-to-mutate other-modules)
              (build-module-paths
               "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/mutate/test_package"
               "lib"
               "test"
               "test-my-defines.rkt"
               "test-my-defines.rkt"
               empty)])
  (run-mutation test-module module-to-mutate other-modules if-swap*))


 
