#lang racket

#;(provide
   (contract-out [extract-fully-expanded-program (-> path-string? any/c)]
                 [extract-syntax                 (->* (path-string?) (namespace?) any/c)]
                 #;[expand-and-disarm              (->* (syntax?) (namespace? path-string?) any/c)])
   expand-and-disarm)

(provide extract-fully-expanded-program
         extract-syntax
         expand-and-disarm)

(require syntax/modread
         debug-scopes)

(define (extract-fully-expanded-program pth)
  (define exp (extract-syntax pth))
  ;; remove this call to syntax->datum
  ;; to get the original syntax object
  ;; use syntax/parse to do pattern matching
  ;; on that in a way that preserves metadata
  ; this doesn't work for box or vector (happens when changing constants)
  (expand-and-disarm exp))

(define (extract-syntax pth)
  (call-with-input-file pth
    (λ (port)
      (port-count-lines! port)
      (with-module-reading-parameterization
        (λ () (read-syntax pth port))))))


(define (expand-and-disarm program-stx #:namespace [namespace (make-base-namespace)] #:directory [directory (current-directory)])
  (parameterize ([current-namespace namespace]
                 [current-directory directory])
    (let loop ([stx (expand program-stx)])
      (cond
        [(syntax? stx)
         (define datum-syntax-stx (datum->syntax (syntax-disarm stx #f)
                                                 (loop (syntax-e (syntax-disarm stx #f)))
                                                 stx
                                                 stx))
         datum-syntax-stx]
        [(pair? stx)
         (cons (loop (car stx))
               (loop (cdr stx)))]
        [else stx]))))
