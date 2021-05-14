#lang racket
(provide make-instrumented-module-runner)
(require custom-load
         syntax/to-string
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         #;"../utilities/read-module.rkt")

(module+ test
  (require ruinit))

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(struct instrumented-module
  (path-string module-path file-path containing-directory stx))

(define/contract (make-instrumented-module-runner main-module
                                                  other-modules-to-instrument
                                                  instrument-module
                                                  #:setup-namespace
                                                  [setup-namespace! (λ _ (void))]
                                                  #:before-main
                                                  [do-before-main! (λ _ (void))]
                                                  #:make-result
                                                  [make-result (λ (ns r) r)])
  (->i ([main-module path-string?]
        [other-modules-to-instrument (listof path-string?)]
        [instrument-module (path-string? syntax? . -> . syntax?)])
       (#:setup-namespace [setup-namespace! (namespace? . -> . void?)]
        #:before-main [do-before-main! (namespace? . -> . any)]
        #:make-result [make-result (namespace? any/c . -> . any/c)])
       #:pre (main-module other-modules-to-instrument)
       (not (member main-module other-modules-to-instrument))
       [result (-> any)])
  (define (make-instrumented-module path-string)
    (define path-string/simplified (path->string (simplify-path path-string)))
    (define module-path `(file ,path-string/simplified))
    (define file-path (module-path->path module-path))
    (define-values (module-containing-directory ___1 ___2)
      (split-path file-path))
    (define module-stx/instrumented (instrument-module path-string
                                                       (read-module file-path)))
    (instrumented-module path-string/simplified
                         module-path
                         file-path
                         module-containing-directory
                         module-stx/instrumented))
  (define main/instrumented (make-instrumented-module main-module))
  (define others/instrumented
    (map make-instrumented-module other-modules-to-instrument))
  ;; Modules must be loaded in order such that loading one module doesn't
  ;; cause another one to be loaded before it gets instrumented
  (define others/instrumented/ordered
    (order-by-dependencies others/instrumented
                           instrumented-module-path-string))
  (define others+main/instrumented/ordered
    (append others/instrumented/ordered
            (list main/instrumented)))
  (define ns (make-base-namespace))
  (setup-namespace! ns)
  (define (run)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure
                    ;; instrumented versions are loaded
                    (make-custom-load/use-compiled
                     #:blacklist
                     (curryr member
                             (map instrumented-module-file-path
                                  others+main/instrumented/ordered)))]
                   [current-namespace ns])
      ;; Eval the instrumented modules one at a time
      (for ([m (in-list others+main/instrumented/ordered)])
        (parameterize
            ;; Ensure relative load paths work
            ([current-load-relative-directory
              (instrumented-module-containing-directory m)]
             [current-module-declare-name
              (module-path-resolve (instrumented-module-module-path m))]
             [current-directory
              (instrumented-module-containing-directory m)])
          (eval (instrumented-module-stx m))))
      ;; Run the main module
      (parameterize
          ;; Ensure relative load paths work
          ([current-load-relative-directory
            (instrumented-module-containing-directory main/instrumented)]
           [current-directory
            (instrumented-module-containing-directory main/instrumented)])
        (do-before-main! ns)
        (define result
          (eval
           `(require ,(instrumented-module-module-path main/instrumented))))
        (make-result ns result))))
  run)
;; (listof path?) -> (listof path?)
;; Order the given set of modules such that every module in the list
;; only depends on modules earlier in the list than itself.
;;
;; Example: given modules A, B, C, D
;; A depends on B, D
;; B depends on D, C
;; D depends on C
;; Expected result: '(C D B A)
(define (order-by-dependencies modules [get-path-string identity])
  (->i ([modules (listof any/c)])
       ([get-path-string (any/c . -> . path-string?)])
       [result (listof any/c)]
       #:post (modules result)
       (for/and ([m (in-list result)]
                 [modules-before (in-list (prefixes result))])
         (set-empty?
          (set-subtract (module-dependencies m modules get-path-string)
                        modules-before))))
  (define dependencies-by-module
    (make-immutable-hash
     (map (λ (m)
            (cons m (module-dependencies m modules get-path-string)))
          modules)))
  (define ((all-dependencies-in? modules-seen) m)
    (set-empty? (set-subtract (hash-ref dependencies-by-module m)
                              modules-seen)))
  (let loop ([module-order-so-far empty]
             [modules-remaining modules])
    (cond [(empty? modules-remaining)
           module-order-so-far]
          [else
           (define feasible-modules
             (filter (all-dependencies-in? module-order-so-far)
                     modules-remaining))
           (when (empty? feasible-modules)
             (error 'order-by-dependencies
                    "Module dependency cycle found in ~v"
                    (get-path-string modules)))
           (loop (append module-order-so-far feasible-modules)
                 (set-subtract modules-remaining feasible-modules))])))
;; Produces a list of the list-prefixes of l, including l itself as
;; the last element
(define (prefixes l)
  (append (for/list ([i (in-range (length l))])
            (take l i))
          (list l)))
(define/contract (module-dependencies m possible-depends
                                      [get-path-string identity])
  (->i ([m any/c]
        [possible-depends (listof any/c)])
       ([get-path-string (any/c . -> . string?)])
       [result (possible-depends)
               (and/c (listof any/c)
                      (curryr subset? possible-depends))])
  (define module-stx (read-module (get-path-string m)))
  (define module-str (syntax->string module-stx))
  (define (module-mentions? other-module)
    (define-values (__1 basename __2)
      (split-path (get-path-string other-module)))
    (string-contains? module-str (path->string basename)))
  (filter module-mentions? (set-subtract possible-depends
                                         (list m))))
