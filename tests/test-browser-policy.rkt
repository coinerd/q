#lang racket

;; tests/test-browser-policy.rkt — Browser policy engine tests
;;
;; TDD test suite for browser/policy.rkt.
;; 43 test cases covering URL validation, IP classification,
;; domain allowlist, action risk, and composed policy checks.

(require rackunit
         "../browser/policy.rkt"
         "../browser/types.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Helper: validate returns (values ok? reason) → we need a wrapper
;; ---------------------------------------------------------------------------

(define (url-ok? url settings)
  (define-values (ok? _) (validate-browser-url url settings))
  ok?)

(define (url-reason url settings)
  (define-values (_ reason) (validate-browser-url url settings))
  reason)

;; ---------------------------------------------------------------------------
;; Helper: settings for testing
;; ---------------------------------------------------------------------------

(define test-settings
  (make-browser-policy-settings
   #:allowed-schemes '("https" "http")
   #:allowed-domains '()
   #:allowed-localhost-ports '(3000 8080 5173)
   #:blocked-private-networks? #t
   #:blocked-paths '("/admin" "/debug" "/internal")))

(define strict-settings
  (make-browser-policy-settings
   #:allowed-schemes '("https")
   #:allowed-domains '("example.com" "docs.racket-lang.org")
   #:allowed-localhost-ports '()
   #:blocked-private-networks? #t
   #:blocked-paths '()))

(define permissive-settings
  (make-browser-policy-settings
   #:allowed-schemes '("https" "http" "file")
   #:allowed-domains '()
   #:allowed-localhost-ports '(3000 8080)
   #:blocked-private-networks? #f
   #:blocked-paths '()))

;; ---------------------------------------------------------------------------
;; 1. Scheme validation (10 tests)
;; ---------------------------------------------------------------------------

(test-case "scheme: https allowed"
  (check-true (url-ok? "https://example.com" test-settings)))

(test-case "scheme: http allowed"
  (check-true (url-ok? "http://example.com" test-settings)))

(test-case "scheme: file:// blocked"
  (check-false (url-ok? "file:///etc/passwd" test-settings)))

(test-case "scheme: data: blocked"
  (check-false (url-ok? "data:text/html,<h1>hi</h1>" test-settings)))

(test-case "scheme: javascript: blocked"
  (check-false (url-ok? "javascript:alert(1)" test-settings)))

(test-case "scheme: ftp blocked"
  (check-false (url-ok? "ftp://files.example.com" test-settings)))

(test-case "scheme: strict settings only allow https"
  (check-false (url-ok? "http://example.com" strict-settings)))

(test-case "scheme: permissive STILL blocks file:// (absolute deny)"
  (check-false (url-ok? "file:///tmp/test.html" permissive-settings)))

(test-case "absolute deny: data: blocked even when data in allowed-schemes"
  (define data-allowed-settings
    (make-browser-policy-settings #:allowed-schemes '("https" "http" "data")))
  (check-false (url-ok? "data:text/html,<h1>hi</h1>" data-allowed-settings)))

(test-case "absolute deny: javascript: blocked even when javascript in allowed-schemes"
  (define js-allowed-settings
    (make-browser-policy-settings #:allowed-schemes '("https" "http" "javascript")))
  (check-false (url-ok? "javascript:alert(1)" js-allowed-settings)))

(test-case "absolute-denied-scheme? predicate"
  (check-not-false (absolute-denied-scheme? "file"))
  (check-not-false (absolute-denied-scheme? "FILE"))
  (check-not-false (absolute-denied-scheme? "data"))
  (check-not-false (absolute-denied-scheme? "javascript"))
  (check-false (absolute-denied-scheme? "https"))
  (check-false (absolute-denied-scheme? "http")))

(test-case "absolute-denied-schemes constant has 3 entries"
  (check-equal? (length absolute-denied-schemes) 3))

(test-case "scheme: empty URL rejected"
  (check-false (url-ok? "" test-settings)))

(test-case "scheme: malformed URL rejected"
  (check-false (url-ok? "not-a-url" test-settings)))

;; ---------------------------------------------------------------------------
;; 2. IP classification (10 tests)
;; ---------------------------------------------------------------------------

(test-case "IP: public IP classified as public"
  (check-equal? (classify-ip "8.8.8.8") 'public))

(test-case "IP: 10.x private"
  (check-equal? (classify-ip "10.0.0.1") 'private))

(test-case "IP: 172.16.x private"
  (check-equal? (classify-ip "172.16.0.1") 'private))

(test-case "IP: 172.31.x private"
  (check-equal? (classify-ip "172.31.255.255") 'private))

(test-case "IP: 172.32.x NOT private"
  (check-equal? (classify-ip "172.32.0.1") 'public))

(test-case "IP: 192.168.x private"
  (check-equal? (classify-ip "192.168.1.1") 'private))

(test-case "IP: 127.x loopback"
  (check-equal? (classify-ip "127.0.0.1") 'loopback))

(test-case "IP: 169.254.x link-local"
  (check-equal? (classify-ip "169.254.1.1") 'link-local))

(test-case "IP: 169.254.169.254 cloud-metadata"
  (check-equal? (classify-ip "169.254.169.254") 'cloud-metadata))

(test-case "IP: non-IP string → unknown"
  (check-equal? (classify-ip "not-an-ip") 'unknown))

;; ---------------------------------------------------------------------------
;; 3. Private network / localhost blocking (6 tests)
;; ---------------------------------------------------------------------------

(test-case "network: private IP blocked when blocked-private-networks?=#t"
  (check-false (url-ok? "http://10.0.0.1/path" test-settings)))

(test-case "network: 192.168.x blocked"
  (check-false (url-ok? "http://192.168.1.1/path" test-settings)))

(test-case "network: cloud metadata 169.254.169.254 blocked"
  (check-false (url-ok? "http://169.254.169.254/latest/meta-data/" test-settings)))

(test-case "network: localhost with allowed port allowed"
  (check-true (url-ok? "http://localhost:3000/app" test-settings)))

(test-case "network: localhost with disallowed port blocked"
  (check-false (url-ok? "http://localhost:9000/app" test-settings)))

(test-case "network: 127.0.0.1 with allowed port allowed"
  (check-true (url-ok? "http://127.0.0.1:8080/api" test-settings)))

;; ---------------------------------------------------------------------------
;; 4. Domain allowlist (4 tests)
;; ---------------------------------------------------------------------------

(test-case "domain: strict allows example.com"
  (check-true (url-ok? "https://example.com/page" strict-settings)))

(test-case "domain: strict blocks unknown.com"
  (check-false (url-ok? "https://unknown.com/page" strict-settings)))

(test-case "domain: subdomain of allowed domain blocked (exact match)"
  (check-false (url-ok? "https://sub.example.com/page" strict-settings)))

(test-case "domain: empty allowlist allows all public domains"
  (check-true (url-ok? "https://any-public-site.com/page" test-settings)))

;; ---------------------------------------------------------------------------
;; 5. Path blocking (3 tests)
;; ---------------------------------------------------------------------------

(test-case "path: /admin blocked"
  (check-false (url-ok? "https://example.com/admin" test-settings)))

(test-case "path: /debug blocked"
  (check-false (url-ok? "https://example.com/debug/panel" test-settings)))

(test-case "path: /internal blocked"
  (check-false (url-ok? "https://example.com/internal/api" test-settings)))

;; ---------------------------------------------------------------------------
;; 6. Action risk classification (5 tests)
;; ---------------------------------------------------------------------------

(test-case "action: navigate is 'navigate"
  (check-equal? (classify-browser-action (browser-action-navigate "https://x.com" "load")) 'navigate))

(test-case "action: click is 'write"
  (check-equal? (classify-browser-action (browser-action-click "#btn" "left")) 'write))

(test-case "action: type is 'write"
  (check-equal? (classify-browser-action (browser-action-type "#inp" "hello" #f)) 'write))

(test-case "action: scroll is 'read"
  (check-equal? (classify-browser-action (browser-action-scroll "down" 300)) 'read))

(test-case "action: press is 'write"
  (check-equal? (classify-browser-action (browser-action-press "Enter" #f)) 'write))

;; ---------------------------------------------------------------------------
;; 7. Composed policy check (5 tests)
;; ---------------------------------------------------------------------------

(test-case "composed: valid target + read action passes"
  (check-not-exn
   (lambda ()
     (check-browser-policy!
      (browser-target "https://example.com" #f #f #f)
      (browser-action-scroll "down" 100)
      test-settings))))

(test-case "composed: blocked URL raises q-browser-error"
  (check-exn q-browser-error?
             (lambda ()
               (check-browser-policy!
                (browser-target "file:///etc/passwd" #f #f #f)
                (browser-action-navigate "file:///etc/passwd" "load")
                test-settings))))

(test-case "composed: blocked path raises error"
  (check-exn q-browser-error?
             (lambda ()
               (check-browser-policy!
                (browser-target "https://example.com/admin" #f #f #f)
                (browser-action-navigate "https://example.com/admin" "load")
                test-settings))))

(test-case "composed: write action on localhost allowed port"
  (check-not-exn
   (lambda ()
     (check-browser-policy!
      (browser-target "http://localhost:3000/form" #f #f #f)
      (browser-action-click "#submit" "left")
      test-settings))))

(test-case "composed: private IP blocked even for read"
  (check-exn q-browser-error?
             (lambda ()
               (check-browser-policy!
                (browser-target "http://10.0.0.1/page" #f #f #f)
                (browser-action-scroll "down" 100)
                test-settings))))
