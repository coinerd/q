#lang racket/base
;; runtime/memory/backends/external-protocol.rkt — External backend adapter skeleton
;;
;; Disabled by default. Never called unless explicitly configured.
;; All outbound payloads are recursively redacted and calls are timeout-bound.

(require racket/match
         "../types.rkt"
         "../protocol.rkt"
         "../policy.rkt")

(define current-external-backend-enabled (make-parameter #f))
(define current-external-timeout-ms (make-parameter 5000))

(define (result-error code message [retryable? #f])
  (memory-result #f #f (make-memory-error code message retryable?) (hasheq)))

(define (disabled-error)
  (result-error 'disabled "External backend disabled" #f))

(define redaction-patterns default-blocked-content-patterns)

(define (redact-content content)
  (redact-memory-content content))

(define (redact-jsexpr v)
  (cond
    [(string? v) (redact-content v)]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)])
       (values k (redact-jsexpr val)))]
    [(list? v) (map redact-jsexpr v)]
    [(vector? v) (list->vector (map redact-jsexpr (vector->list v)))]
    [else v]))

(define (call-with-timeout thunk timeout-ms)
  (define ch (make-channel))
  (define worker
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (channel-put ch (cons 'error e)))])
                (channel-put ch (cons 'ok (thunk)))))))
  (define result (sync/timeout (/ timeout-ms 1000.0) ch))
  (cond
    [result
     (kill-thread worker)
     (match result
       [(cons 'ok value) value]
       [(cons 'error e) (raise e)])]
    [else
     (kill-thread worker)
     (error 'external-backend (format "External backend call timed out after ~ams" timeout-ms))]))

(define (ensure-memory-result v #:default-value [default-value #f])
  (cond
    [(memory-result? v) v]
    [else (memory-result #t (or v default-value) #f (hasheq))]))

(define (external-call method payload transport-fn timeout-ms #:default-value [default-value #f])
  (cond
    [(not (current-external-backend-enabled)) (disabled-error)]
    [(not (memory-external-write-allowed?))
     (result-error 'safe-mode "External backend calls are blocked in safe mode" #f)]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (define msg (exn-message e))
                                  (result-error
                                   (if (regexp-match? #px"timed out" msg) 'timeout 'transport-error)
                                   msg
                                   (regexp-match? #px"timed out" msg)))])
       (ensure-memory-result
        (call-with-timeout (lambda () (transport-fn method (redact-jsexpr payload))) timeout-ms)
        #:default-value default-value))]))

(define (make-external-backend name transport-fn #:timeout-ms [timeout-ms #f])
  (define effective-timeout (or timeout-ms (current-external-timeout-ms)))
  (memory-backend
   (format "external(~a)" name)
   (lambda (item)
     (external-call 'store
                    (hash 'id
                          (memory-item-id item)
                          'content
                          (memory-item-content item)
                          'scope
                          (memory-item-scope item)
                          'type
                          (memory-item-type item))
                    transport-fn
                    effective-timeout))
   ;; F37: Design: retrieve/list return success with empty results when disabled
   ;; (no items to retrieve = valid empty response). store/delete/update return
   ;; error when disabled (write operations must not silently succeed).
   (lambda (query)
     (if (not (current-external-backend-enabled))
         (memory-result #t '() #f (hasheq))
         (external-call 'retrieve
                        (hash 'text
                              (memory-query-text query)
                              'scope
                              (memory-query-scope query)
                              'project-root
                              (memory-query-project-root query)
                              'session-id
                              (memory-query-session-id query)
                              'limit
                              (memory-query-limit query))
                        transport-fn
                        effective-timeout
                        #:default-value '())))
   (lambda (id patch)
     (external-call 'update (hash 'id id 'patch patch) transport-fn effective-timeout))
   (lambda (id scope)
     (external-call 'delete (hash 'id id 'scope scope) transport-fn effective-timeout))
   (lambda (query)
     (if (not (current-external-backend-enabled))
         (memory-result #t '() #f (hasheq))
         (external-call 'list
                        (hash 'scope
                              (memory-query-scope query)
                              'project-root
                              (memory-query-project-root query)
                              'session-id
                              (memory-query-session-id query)
                              'limit
                              (memory-query-limit query))
                        transport-fn
                        effective-timeout
                        #:default-value '())))
   (lambda () (current-external-backend-enabled))
   (lambda (policy)
     (if (not (current-external-backend-enabled))
         (memory-result #t #f #f (hasheq))
         (external-call 'manage (hash 'policy policy) transport-fn effective-timeout)))))

(provide current-external-backend-enabled
         current-external-timeout-ms
         make-external-backend
         redact-content
         redact-jsexpr)
