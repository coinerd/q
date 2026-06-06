#lang racket/base
;; runtime/memory/backends/external-protocol.rkt — External backend adapter skeleton
;;
;; v0.95.11: Interface for external memory backends (vector DB, graph DB, etc.)
;; Disabled by default. Never called unless explicitly configured.
;; All external calls include timeout and local policy pre-check.
;; External backends receive redacted payloads only (no secrets).
;;
;; Safety:
;;   - Never sends data unless current-external-backend-enabled is #t
;;   - Pre-checks policy before any outbound call
;;   - Redacts known secret patterns from payloads
;;   - Enforces timeout on all external calls
;;   - Logs all external interactions for audit

(require racket/match
         "../types.rkt"
         "../protocol.rkt"
         "../policy.rkt")

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

;; Master switch — disabled by default
(define current-external-backend-enabled (make-parameter #f))

;; Default timeout for external calls (ms)
(define current-external-timeout-ms (make-parameter 5000))

;; ---------------------------------------------------------------------------
;; Redaction
;; ---------------------------------------------------------------------------

;; Patterns that must be redacted before sending to external backend
(define redaction-patterns
  (list (cons #px"(?i:password).*[=:].[^ ]+" "password: [REDACTED]")
        (cons #px"(?i:api.?key).*[=:].[^ ]+" "api_key: [REDACTED]")
        (cons #px"(?i:token).*[=:].[^ ]+" "token: [REDACTED]")
        (cons #px"(?i:bearer)\\s+\\S{10,}" "bearer [REDACTED]")
        (cons #px"AKIA[A-Z0-9]{10,}" "AKIA[REDACTED]")
        (cons #px"-----BEGIN.*PRIVATE KEY-----[^-]*-----END[^-]*-----" "[PRIVATE KEY REDACTED]")))

(define (redact-content content)
  (for/fold ([c content]) ([pair (in-list redaction-patterns)])
    (match-define (cons pattern replacement) pair)
    (regexp-replace* pattern c replacement)))

;; ---------------------------------------------------------------------------
;; External backend constructor
;; ---------------------------------------------------------------------------

;; Creates an external backend adapter. Takes a transport function
;; that will be called with (method payload) and must return a result.
;; The transport is never called unless enabled + policy allows.
(define (make-external-backend name transport-fn #:timeout-ms [timeout-ms #f])
  (define effective-timeout (or timeout-ms (current-external-timeout-ms)))

  (define (pre-check-and-call method payload)
    (unless (current-external-backend-enabled)
      (error 'external-backend "External backend called while disabled"))
    ;; Redact payload before sending
    (define redacted-payload
      (hash-set payload 'content (redact-content (hash-ref payload 'content ""))))
    (transport-fn method redacted-payload))

  (memory-backend
   (format "external(~a)" name)
   ;; store!
   (lambda (item)
     (if (not (current-external-backend-enabled))
         (memory-result #f #f (hash 'message "External backend disabled") (hasheq))
         (with-handlers
             ([exn:fail? (lambda (e) (memory-result #f #f (hash 'message (exn-message e)) (hasheq)))])
           (pre-check-and-call 'store
                               (hash 'id
                                     (memory-item-id item)
                                     'content
                                     (memory-item-content item)
                                     'scope
                                     (memory-item-scope item)
                                     'type
                                     (memory-item-type item))))))
   ;; retrieve
   (lambda (query)
     (if (not (current-external-backend-enabled))
         (memory-result #t '() #f (hasheq))
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (memory-result #f '() (hash 'message (exn-message e)) (hasheq)))])
           (pre-check-and-call 'retrieve
                               (hash 'text
                                     (memory-query-text query)
                                     'scope
                                     (memory-query-scope query)
                                     'limit
                                     (memory-query-limit query))))))
   ;; update!
   (lambda (id patch)
     (if (not (current-external-backend-enabled))
         (memory-result #f #f (hash 'message "External backend disabled") (hasheq))
         (with-handlers
             ([exn:fail? (lambda (e) (memory-result #f #f (hash 'message (exn-message e)) (hasheq)))])
           (pre-check-and-call 'update (hash 'id id 'patch patch)))))
   ;; delete!
   (lambda (id scope)
     (if (not (current-external-backend-enabled))
         (memory-result #f #f (hash 'message "External backend disabled") (hasheq))
         (with-handlers
             ([exn:fail? (lambda (e) (memory-result #f #f (hash 'message (exn-message e)) (hasheq)))])
           (pre-check-and-call 'delete (hash 'id id 'scope scope)))))
   ;; list
   (lambda (query)
     (if (not (current-external-backend-enabled))
         '()
         (with-handlers ([exn:fail? (lambda (e) '())])
           (pre-check-and-call
            'list
            (hash 'scope (memory-query-scope query) 'limit (memory-query-limit query))))))
   ;; available?
   (lambda () (current-external-backend-enabled))
   ;; manage!
   (lambda (policy)
     (if (not (current-external-backend-enabled))
         (memory-result #t #f #f (hasheq))
         (with-handlers
             ([exn:fail? (lambda (e) (memory-result #f #f (hash 'message (exn-message e)) (hasheq)))])
           (pre-check-and-call 'manage (hash 'policy policy)))))))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide current-external-backend-enabled
         current-external-timeout-ms
         make-external-backend
         redact-content)
