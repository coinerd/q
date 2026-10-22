#lang racket/base

;; Trusted, dynamically scoped worker policy. Never derive callbacks from JSON.
(require racket/dict)
(provide (struct-out worker-tool-policy)
         current-worker-tool-policy
         filter-worker-tools
         worker-tool-denial
         capture-worker-tool-policy
         config-worker-tool-policy)

(struct worker-tool-policy (filter admit)
  #:transparent
  #:guard
  (lambda (filter admit who)
    (unless (and (procedure-arity-includes? filter 1) (procedure-arity-includes? admit 2))
      (raise-argument-error who "unary filter and binary admit procedures" (list filter admit)))
    (values filter admit)))
(define current-worker-tool-policy
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v) (worker-tool-policy? v))
                      (raise-argument-error 'current-worker-tool-policy "#f or worker-tool-policy" v))
                    v)))

;; An uninterned key survives in-memory config copies but cannot be forged by
;; project/model JSON. Fresh sessions always replace it, including with #f.
(define policy-key (gensym 'trusted-worker-tool-policy))
(define (capture-worker-tool-policy config)
  (dict-set config policy-key (current-worker-tool-policy)))
(define (config-worker-tool-policy config)
  (dict-ref config policy-key #f))

(define (filter-worker-tools tools [policy (current-worker-tool-policy)])
  (if (not policy)
      tools
      (with-handlers ([(lambda (_) #t) (lambda (_) '())])
        (define source (or tools '()))
        (define result
          (parameterize ([current-worker-tool-policy policy])
            ((worker-tool-policy-filter policy) source)))
        ;; Projection may remove schemas, never add or modify them.
        (if (and (list? result)
                 (andmap (lambda (schema) (and (hash? schema) (member schema source))) result))
            (for/list ([schema (in-list source)]
                       #:when (member schema result))
              schema)
            '()))))

(define (worker-tool-denial name args [policy (current-worker-tool-policy)])
  (and policy
       (with-handlers ([(lambda (_) #t) (lambda (_) "worker policy authorization callback failed")])
         (define result
           (parameterize ([current-worker-tool-policy policy])
             ((worker-tool-policy-admit policy) name args)))
         (cond
           [(not result) #f]
           [(and (string? result) (positive? (string-length result))) result]
           [else "worker policy authorization returned an invalid denial"]))))
