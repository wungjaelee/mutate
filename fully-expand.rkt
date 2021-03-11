#lang racket

(provide
 (contract-out [extract-fully-expanded-program (-> path-string? any/c)]
               [extract-syntax                 (->* (path-string?) (namespace?) any/c)]
               [expand-and-disarm              (->* (syntax?) (namespace? path-string?) any/c)]))

(require syntax/modread)

(define (extract-fully-expanded-program pth)
  (define new-namespace (make-base-namespace))
  (define exp (extract-syntax pth new-namespace))
  ;; remove this call to syntax->datum
  ;; to get the original syntax object
  ;; use syntax/parse to do pattern matching
  ;; on that in a way that preserves metadata
  ; this doesn't work for box or vector (happens when changing constants)
  (expand-and-disarm exp new-namespace))

(define (extract-syntax pth [namespace (make-base-namespace)])
  (parameterize ([current-namespace namespace])
    (call-with-input-file pth
      (λ (port)
        (port-count-lines! port)
        (with-module-reading-parameterization
          (λ () (read-syntax pth port)))))))

(define (expand-and-disarm program-stx [namespace (make-base-namespace)] [directory (current-directory)])
  (parameterize ([current-namespace namespace]
                 [current-directory directory])
    (let loop ([stx (expand program-stx)])
      (cond
        [(pair? stx)
         (cons (loop (car stx))
               (loop (cdr stx)))]
        [(syntax? stx)
         (datum->syntax (syntax-disarm stx #f)
                        (loop (syntax-e (syntax-disarm stx #f)))
                        (syntax-disarm stx #f)
                        (syntax-disarm stx #f))]
        [else stx]))))
