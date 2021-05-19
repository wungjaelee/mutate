#lang racket

(define (get-length val)
  (if (list? val) (length val) #f))

(define (get-length-error val)
  (if (list? val) #f (length val)))



(module+ test
  (require rackunit)
  ; my-defines will be imported by mutator
  (define val-maybe-error (get-length 3)) ; #f or (error "wrong-id") if get-length's then else is swapped
  (check-false val-maybe-error) ; pass if get-length is not mutated, error if get-length's then else is swapped
  
 
  (define val-not-ok (get-length-error 3))
  (check-false val-not-ok) ; error if get-length-error is not mutated, pass if then-else is swapped
  
  (define-values (v1 v2) (values (get-length 3) 2))
  (check-false v1) ; error if not mutated, pass if mutated
  (check-equal? v2 2) ; error if not mutated, pass if mutated

  (define (check-permutation? e l)
    (check-not-false (member e (permutations l))))

  (check-permutation? '(3 1 2) '(1 2 3))
  (check-permutation? (length 3) '(1 2 3))

  (check-true (equal? 3 3))
  )
  