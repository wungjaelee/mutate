#lang racket
(require "mutated.rkt"
         "mutator-lib.rkt"
         "mutate-expr.rkt"
         "mutate-program.rkt"
         "mutate-util.rkt"
         ;"my-logger.rkt"
         syntax/parse)

(provide wrap-check-equal
         wrap-check-equal2)

(define-simple-mutator (wrap-check-equal stx)
  #:pattern ({~literal check-equal?} e1 e2)
  (datum->syntax stx
                 `(begin (,#'log-test-info "(check-equal? ~a ~a)\n" ',#'e1 ',#'e2)
                         ,stx)
                 stx
                 stx))

(define-simple-mutator (wrap-check-equal2 stx)
  #:pattern ({~literal check-equal?} e1 e2)
  #'(begin (log-test-info "(check-equal? ~a ~a)\n" e1 e2)
           stx))

