#lang racket

(require "my-logger.rkt")
(provide define)

(define-syntax define
  (syntax-rules ()
    [(define (fn-id args ...) body)
     (define-syntax fn-id
       (lambda (stx)
         (syntax-case stx ()
           [(fn-id params (... ...))
            #;#'((lambda (args ...) body) params (... ...))
            #'(with-handlers ([exn? (λ (e) (log-test-info "~a in test suite raised an error ~a" 'fn-id e))])
                ((lambda (args ...) body) params (... ...)))])))]
    [(define id body)
     (define-syntax id
       (lambda (stx)
         (syntax-case stx ()
           [id (identifier? #'id)
               #;#'body
               #'(with-handlers ([exn? (λ (e) (log-test-info "asdfasdf x~a" e) 'dummy-val)])
                   body)])))]))

#;(define-syntax define-values
  (syntax-rules ()
    [(define-values (id ...))]))

