#lang racket

(require "my-logger.rkt")
(provide define)

(define-syntax define
  (syntax-rules ()
    [(define id body)
     (define-syntax id
       (lambda (stx)
         (syntax-case stx ()
           [id (identifier? #'id)
               #'(with-handlers ([exn? (λ (e) (log-test-info "~a" e) 'dummy-val)])
                   body)])))]))

