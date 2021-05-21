#lang racket
(provide get-length
         get-length-error)

(define (get-length val)
  (if (list? val) (length val) #f))

(define (get-length-error val)
  (if (list? val) #f (length val)))
  