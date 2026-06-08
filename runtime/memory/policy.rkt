#lang racket/base
;; runtime/memory/policy.rkt — Memory write/read policy gating
;;
;; Controls what memory operations are allowed:
;;   - Memory must be enabled in config (backend present)
;;   - Sensitivity levels gate who can store what
;;   - Secret content is blocked from storage entirely
;;   - Scope isolation enforced (session-scoped memory not visible cross-session)
;;   - Token/count budgets limit retrieval volume

(require racket/string
         "types.rkt"
         (only-in "../../util/safe-mode/safe-mode-state.rkt" safe-mode?))

(provide (struct-out memory-policy)
         memory-policy?
         default-memory-policy
         default-blocked-content-patterns
         make-memory-policy
         policy-allows-store?
         policy-allows-retrieve?
         policy-allows-delete?
         policy-check-content-safety
         policy-within-budget?
         effective-memory-scope
         policy-user-scope-enabled?
         policy-allows-scope?
         memory-safe-mode-active?
         memory-persistent-write-allowed?
         memory-external-write-allowed?
         redact-memory-content
         redacted-memory-snippet
         safe-memory-content?)

;; ---------------------------------------------------------------------------
;; Policy struct
;; ---------------------------------------------------------------------------

(struct memory-policy
        (max-items-per-session ; nat — max stored items per session scope
         max-retrieve-count ; nat — max items returned per query
         max-content-length ; nat — max chars per content field
         allowed-sensitivities ; (listof sensitivity?) — which levels can be stored
         blocked-content-patterns ; (listof regexp) — patterns that prevent storage
         allow-delete? ; bool — whether delete is allowed
         user-scope-enabled?) ; bool — whether user/global memory is allowed
  #:transparent)

;; Content-level secret detectors used by the default store policy.
;; These are deliberately broad because memory is long-lived and may be
;; injected back into prompts. Callers that need a narrower policy can pass an
;; explicit #:blocked-content-patterns list to make-memory-policy.
(define default-blocked-content-patterns
  (list
   #px"(?i:api[_-]?key\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:access[_-]?token\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:secret[_-]?key\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:client[_-]?secret[\\\"']*\\s*[:=]\\s*[\\\"']?[^\\s,}]+)"
   #px"(?i:password\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:bearer\\s+[A-Za-z0-9._~+/=-]{12,})"
   #px"(?i:token\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:oauth[_-]?token\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:refresh[_-]?token\\s*[:=]\\s*[^\\s]+)"
   #px"(?i:private_key\\s*[:=])"
   #px"(?s:-----BEGIN [A-Z ]*PRIVATE KEY-----.*-----END [A-Z ]*PRIVATE KEY-----)"
   #px"(?m:^\\s*[A-Za-z_][A-Za-z0-9_]*_(KEY|TOKEN|SECRET|PASSWORD)\\s*=\\s*[^\\s]+)"
   #px"sk-[A-Za-z0-9_-]{8,}"
   #px"gh[pousr]_[A-Za-z0-9_]{12,}"
   #px"AKIA[A-Z0-9]{10,}"
   ;; Multi-line .env holistic pattern (P3-1)
   #px"(?im:^[A-Za-z_][A-Za-z0-9_]*(?:_KEY|_TOKEN|_SECRET|_PASSWORD|_URL|_HOST|_PORT)\\s*[:=]\\s*[^\\s]+)"))

(define default-memory-policy
  (memory-policy 100 ; max 100 items per session
                 20 ; max 20 items per query
                 10000 ; max 10k chars per content
                 '(public internal
                          sensitive) ; secret blocked from storage
                 default-blocked-content-patterns ; content-level secret blocking
                 #t ; delete allowed
                 #f)) ; user/global scope disabled by default

(define (make-memory-policy #:max-items-per-session [max-items 100]
                            #:max-retrieve-count [max-retrieve 20]
                            #:max-content-length [max-content 10000]
                            #:allowed-sensitivities [sensitivities
                                                     '(public internal
                                                              sensitive)]
                            #:blocked-content-patterns [patterns default-blocked-content-patterns]
                            #:allow-delete? [allow-delete #t]
                            #:user-scope-enabled? [user-scope-enabled? #f])
  (memory-policy max-items
                 max-retrieve
                 max-content
                 sensitivities
                 patterns
                 allow-delete
                 user-scope-enabled?))

;; ---------------------------------------------------------------------------
;; Scope helpers
;; ---------------------------------------------------------------------------

(define (effective-memory-scope requested-scope project-root)
  (cond
    [(and requested-scope (memory-scope? requested-scope)) requested-scope]
    [requested-scope requested-scope]
    [project-root 'project]
    [else 'session]))

(define (policy-user-scope-enabled? policy)
  (and (memory-policy? policy) (memory-policy-user-scope-enabled? policy)))

(define (policy-allows-scope? policy scope)
  (and (memory-policy? policy)
       (memory-scope? scope)
       (or (not (eq? scope 'user)) (policy-user-scope-enabled? policy))))

;; ---------------------------------------------------------------------------
;; Safe mode helpers
;; ---------------------------------------------------------------------------

(define (memory-safe-mode-active?)
  (safe-mode?))

(define (memory-persistent-write-allowed?)
  (not (memory-safe-mode-active?)))

(define (memory-external-write-allowed?)
  (not (memory-safe-mode-active?)))

;; ---------------------------------------------------------------------------
;; Redaction helpers
;; ---------------------------------------------------------------------------

(define (safe-memory-content? policy content)
  (and (string? content) (policy-check-content-safety policy content)))

(define (redact-memory-content content [replacement "[REDACTED]"])
  (cond
    [(not (string? content)) ""]
    [else
     (for/fold ([c content]) ([pattern (in-list default-blocked-content-patterns)])
       (regexp-replace* pattern c replacement))]))

(define (redacted-memory-snippet content [max-len 80])
  (define redacted (redact-memory-content (if (string? content) content "")))
  ;; M13-F13: Collapse whitespace before truncation to avoid leaking context
  ;; fragments around redacted content. Remove adjacent material to [REDACTED].
  (define collapsed (regexp-replace* #px"\\s+" redacted " "))
  ;; Elide text directly adjacent to [REDACTED] to avoid partial secret context
  (define elided (regexp-replace* #px"\\S*[REDACTED]\\S*" collapsed "[REDACTED]"))
  (define trimmed (string-trim elided))
  (if (<= (string-length trimmed) max-len)
      trimmed
      (string-append (substring trimmed 0 (- max-len 3)) "...")))

;; ---------------------------------------------------------------------------
;; Policy checks
;; ---------------------------------------------------------------------------

;; Can this item be stored under this policy?
(define (policy-allows-store? policy item)
  (and (memory-policy? policy)
       (valid-memory-item? item)
       (policy-allows-scope? policy (memory-item-scope item))
       ;; Sensitivity check
       (memq (hash-ref (memory-item-validity item) 'sensitivity 'public)
             (memory-policy-allowed-sensitivities policy))
       ;; Content length check
       (<= (string-length (memory-item-content item)) (memory-policy-max-content-length policy))
       ;; Content safety check
       (policy-check-content-safety policy (memory-item-content item))))

;; Is content safe to store? (no blocked patterns)
(define (policy-check-content-safety policy content)
  (and (memory-policy? policy)
       (string? content)
       (for/and ([pattern (in-list (memory-policy-blocked-content-patterns policy))])
         (not (regexp-match? pattern content)))))

;; Is retrieval allowed under budget?
(define (policy-allows-retrieve? policy requested-count)
  (and (memory-policy? policy)
       (integer? requested-count)
       (<= 0 requested-count (memory-policy-max-retrieve-count policy))))

;; Is delete allowed?
(define (policy-allows-delete? policy)
  (and (memory-policy? policy) (memory-policy-allow-delete? policy)))

;; Check if result set is within budget
(define (policy-within-budget? policy items)
  (and (memory-policy? policy) (<= (length items) (memory-policy-max-retrieve-count policy))))
