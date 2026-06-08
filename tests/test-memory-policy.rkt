#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         racket/file
         racket/string
         "../runtime/memory/policy.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/service.rkt"
         "../runtime/safe-mode.rkt")

(define (item content #:sensitivity [sensitivity 'public] #:scope [scope 'project])
  (memory-item "id"
               'semantic
               scope
               content
               (hasheq 'tags '() 'source 'tool 'project-root "/tmp/project" 'session-id "sess")
               (hasheq 'sensitivity sensitivity 'confidence 1.0)
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z"))

(test-case "default memory policy exists"
  (check-true (memory-policy? default-memory-policy)))

(test-case "default scope is project when project root is available"
  (check-equal? (effective-memory-scope #f "/tmp/project") 'project))

(test-case "default scope falls back to session without project root"
  (check-equal? (effective-memory-scope #f #f) 'session))

(test-case "user scope is disabled by default"
  (check-false (policy-allows-scope? default-memory-policy 'user))
  (check-true (policy-allows-scope? (make-memory-policy #:user-scope-enabled? #t) 'user)))

(test-case "default policy blocks private key blocks"
  (check-false (policy-allows-store?
                default-memory-policy
                (item "-----BEGIN PRIVATE KEY-----\nabc123\n-----END PRIVATE KEY-----"))))

(test-case "default policy blocks env-style credentials"
  (check-false (policy-allows-store? default-memory-policy (item "DATABASE_PASSWORD=hunter2")))
  (check-false (policy-allows-store? default-memory-policy (item "SERVICE_API_KEY=abc123"))))

(test-case "default policy blocks credential json and oauth secrets"
  (check-false (policy-allows-store? default-memory-policy (item "{\"client_secret\":\"abc123\"}")))
  (check-false (policy-allows-store? default-memory-policy (item "oauth_token=abcdef123456"))))

(test-case "redacted snippets never expose raw secret values"
  (define snippet (redacted-memory-snippet "API_KEY=sk-super-secret-value"))
  (check-false (regexp-match? #px"sk-super-secret-value" snippet))
  (check-true (regexp-match? #px"REDACTED" snippet)))

(test-case "safe mode blocks persistent and external writes"
  (parameterize ([current-safe-mode #t])
    (check-false (memory-persistent-write-allowed?))
    (check-false (memory-external-write-allowed?))))

(test-case "normal mode allows persistent and external writes by policy helper"
  (parameterize ([current-safe-mode #f]
                 [current-safe-mode-config #f])
    (check-true (memory-persistent-write-allowed?))
    (check-true (memory-external-write-allowed?))))

;; ---------------------------------------------------------------------------
;; v0.95.21 W0: G2 — User-scope config wiring tests
;; ---------------------------------------------------------------------------

(test-case "W0 G2: user-scope disabled by default in policy"
  (check-false (policy-user-scope-enabled? default-memory-policy)
               "User scope must be disabled by default"))

(test-case "W0 G2: user-scope blocks 'user scope by default"
  (check-false (policy-allows-scope? default-memory-policy 'user)
               "Default policy must block 'user scope"))

;; ---------------------------------------------------------------------------
;; v0.95.21 W0: G2 — User-scope config wiring gap verification
;; ---------------------------------------------------------------------------

(test-case "W0 G2: make-memory-policy accepts #:user-scope-enabled? #t"
  (define enabled-policy (make-memory-policy #:user-scope-enabled? #t))
  (check-true (policy-user-scope-enabled? enabled-policy))
  (check-true (policy-allows-scope? enabled-policy 'user)))

(test-case "W1 G2: setting-memory-user-scope-enabled? now exists in settings.rkt"
  (define src (file->string (build-path (current-directory) ".." "runtime" "settings.rkt")))
  (check-true (regexp-match? #rx"setting-memory-user-scope-enabled" src)
              "W1: setting-memory-user-scope-enabled? should exist"))

(test-case "W1 G2: update-memory-policy! now exists in service.rkt"
  (define src (file->string (build-path (current-directory) ".." "runtime" "memory" "service.rkt")))
  (check-true (regexp-match? #rx"update-memory-policy" src)
              "W1: update-memory-policy! should exist"))

;; ---------------------------------------------------------------------------
;; v0.95.21 W1: G2 — User-scope wiring functional tests
;; ---------------------------------------------------------------------------

(test-case "W1 G2: update-memory-policy! enables user scope"
  (parameterize ([current-memory-policy default-memory-policy])
    (update-memory-policy! #:user-scope-enabled? #t)
    (check-true (policy-user-scope-enabled? (current-memory-policy)))
    (check-true (policy-allows-scope? (current-memory-policy) 'user))))

(test-case "W1 G2: update-memory-policy! preserves other fields"
  (parameterize ([current-memory-policy default-memory-policy])
    (define orig-max (memory-policy-max-items-per-session (current-memory-policy)))
    (update-memory-policy! #:user-scope-enabled? #t)
    (check-equal? (memory-policy-max-items-per-session (current-memory-policy)) orig-max)))

(test-case "W1 G2: update-memory-policy! with #f disables user scope"
  (parameterize ([current-memory-policy (make-memory-policy #:user-scope-enabled? #t)])
    (check-true (policy-user-scope-enabled? (current-memory-policy)))
    (update-memory-policy! #:user-scope-enabled? #f)
    (check-false (policy-user-scope-enabled? (current-memory-policy)))))
