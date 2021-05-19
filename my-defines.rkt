#lang racket

(require "my-logger.rkt"
         syntax/parse/define
         (for-syntax racket)
         (rename-in racket
                    [define-values @define-values]
                    [define @define]))

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
                 #'(with-handlers ([exn? (λ (e) (log-test-info "DEFINE-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised" 'id e) (error e))])
                     expr)])))]))

(define-syntax (define-values stx)
  (syntax-parse stx
    [(_ (id ...) expr)
     #`(define-syntaxes (id ...)
         (values #,@(build-list (length (syntax->list #'(id ...)))
                                (λ (i) #`(lambda (stx)
                                           (syntax-case stx ()
                                             [_ #'(call-with-values (thunk expr)
                                                                    (lambda vals (list-ref vals #,i)))]))))))]))



(provide define
         define-values)


#;(define-syntax-parser define-values
    [(define-values (id ...) expr)
     #`(@define-values (id ...)
                       (with-handlers ([exn?
                                        (λ (e)
                                          (display "hello")
                                          (log-test-info "DEFINE-VALUES-ID-ERROR: while defining IDENTIFIER ~a ERROR ~a was raised" '(id ...) e)
                                          (define-syntaxes (id ...)
                                            (values #,@(map (λ (x) (λ (stx) (syntax-case stx ()
                                                                              [x (error "define-values-id error")]))) (syntax->list #'(id ...)))))
                                          #;(values #,@(map (λ (_x) #'(error "define-values id error")) (syntax->list #'(id ...)))))])
                         expr))])


