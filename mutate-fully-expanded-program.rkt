#lang racket
(require syntax/parse
         "fully-expand.rkt"
         "fully-expanded-syntax-mutators.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "mutate-expr.rkt"
         "mutate-program.rkt"
         "mutate-util.rkt"
         "top-level-selectors.rkt")

(define swap*
  (apply compose-mutators
         (list plain-lambda-swap*
               if-swap*)))

(define mutate-expr (make-expr-mutator if-swap*))
(define mutate-program (make-program-mutator mutate-expr
                                             select-except-test-module))
(define mutate-program-syntax (syntax-only mutate-program))

(define (make-mutant my-module-stx i)
  (syntax-parse my-module-stx
    [(module name lang (#%module-begin top-level-form ...))
     #:with [mutated-top-level-form ...] (mutate-program-syntax #'[top-level-form ...] i)
     #'(begin (module name lang (#%module-begin mutated-top-level-form ...))
              (require 'name))]))

(define test-program
  (extract-fully-expanded-program "/Users/wungjaelee/Everything/RESEARCH/Programming_Language_Research/santorini/player.rkt"))

(define mutant (make-mutant test-program 0))
(eval-syntax mutant)