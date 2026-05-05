#lang racket/base

;; tests/helpers/ws-test-helpers.rkt — Test helper for working-set context factory
;;
;; W0 scaffolding: provides a basic make-ws-context that satisfies
;; the tests in test-working-set-factory.rkt.
;; In W2, the real implementation will be in runtime/working-set.rkt.

(require racket/list)

(provide make-ws-context)

(define (make-ws-context #:max-entries [max-entries 30] #:max-tokens [max-tokens 15000])
  (let ([entries '()]
        [sem (make-semaphore 1)])
    (lambda (action . args)
      (call-with-semaphore
       sem
       (lambda ()
         (case action
           [(entries) entries]
           [(entry-count) (length entries)]
           [(token-count) (for/sum ([e (in-list entries)]) (hash-ref e 'token-estimate 0))]
           [(max-entries) max-entries]
           [(max-tokens) max-tokens]
           [(reset!) (set! entries '())]
           [(add!)
            (define path (car args))
            (define msg-id (cadr args))
            (define tokens (caddr args))
            ;; Validation
            (unless (string? path)
              (raise-argument-error 'ws-context "string" path))
            (unless (number? tokens)
              (raise-argument-error 'ws-context "number" tokens))
            ;; Create entry
            (define new-entry (hasheq 'path path 'message-id msg-id 'token-estimate tokens))
            ;; Remove existing entry for same path (refresh)
            (define without-existing
              (filter (lambda (e) (not (equal? (hash-ref e 'path) path))) entries))
            ;; Add to front
            (set! entries (cons new-entry without-existing))
            ;; Enforce max-entries (LRU eviction from tail)
            (when (> (length entries) max-entries)
              (set! entries (take entries max-entries)))]
           [(remove!)
            (define path (car args))
            (set! entries (filter (lambda (e) (not (equal? (hash-ref e 'path) path))) entries))]
           [else (raise-argument-error 'ws-context "known action symbol" action)]))))))
