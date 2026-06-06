#lang racket/base

(require rackunit
         "../runtime/memory/policy.rkt"
         "../runtime/memory/types.rkt"
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
