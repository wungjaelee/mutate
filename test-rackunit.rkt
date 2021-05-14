#lang racket

(module+ test
  (require rackunit)
  (check-regexp-match 3 "aaaabbba"))