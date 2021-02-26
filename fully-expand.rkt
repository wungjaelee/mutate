#lang racket

(provide
  (contract-out [extract-fully-expanded-program (-> path-string? any/c)]))

(require syntax/modread)

(define (extract-fully-expanded-program pth)
  (parameterize ([current-namespace (make-base-namespace)])
    (define exp
      (call-with-input-file pth
        (λ (port)
          (port-count-lines! port)
          (with-module-reading-parameterization
            (λ ()
              (read-syntax pth port))))))
    ;; remove this call to syntax->datum
    ;; to get the original syntax object
    ;; use syntax/parse to do pattern matching
    ;; on that in a way that preserves metadata
    ; this doesn't work for box or vector (happens when changing constants)
    (let loop ([stx (expand exp)])
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
