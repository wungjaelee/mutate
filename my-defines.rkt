#lang racket

(require "my-logger.rkt"
         syntax/parse/define
         (rename-in racket [define-values @define-values]))


(define-syntax define
  (syntax-rules ()
    [(define (fn-id args ...) body ...)
     (define-syntax fn-id
       (lambda (stx)
         (syntax-case stx ()
           [(fn-id params (... ...))
            #'((lambda (args ...) body ...) params (... ...))
            #;#'(with-handlers ([exn? (λ (e) (log-test-info "~a in test suite raised an error ~a" 'fn-id e))])
                  ((lambda (args ...) body) params (... ...)))])))]
    [(define id expr)
     (define-syntax id
       (lambda (stx)
         (syntax-case stx ()
           [id (identifier? #'id)
               #;#'body
               #'(with-handlers ([exn? (λ (e) (log-test-info "DEFINE-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised" 'id e) 'dummy-val)])
                   expr)])))]))

(define-syntax-parser define-values
  [(define-values (id ...) expr)
   #`(@define-values (id ...)
                     (with-handlers ([exn?
                                      (λ (e)
                                        (log-test-info "DEFINE-VALUES-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised" '(id ...) e)
                                        (values #,@(map (λ (_x) ''dummy-val) (syntax->list #'(id ...)))))])
                       expr))])




(provide define define-values)
