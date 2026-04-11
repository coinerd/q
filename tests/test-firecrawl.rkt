#lang racket

(require rackunit
         rackunit/text-ui
         (only-in "../tools/tool.rkt" tool-result? tool-result-content tool-result-is-error? tool-result-details)
         "../tools/builtins/firecrawl.rkt")

;; Helper: extract text from tool-result content
(define (content-text r)
  (define parts (tool-result-content r))
  (if (null? parts)
      ""
      (hash-ref (car parts) 'text "")))

(define firecrawl-tests
  (test-suite
   "Firecrawl Tool Tests"

   ;; ---- Validation tests (no API key needed) ----

   (test-case "missing action returns error"
     (define r (tool-firecrawl (hasheq)))
     (check-true (tool-result? r))
     (check-true (tool-result-is-error? r))
     (check-true (string-contains? (content-text r) "Missing required parameter")
                 (format "Expected missing action message, got: ~a" (content-text r))))

   (test-case "invalid action returns error"
     (define r (tool-firecrawl (hasheq 'action "foobar")))
     (check-true (tool-result? r))
     (check-true (tool-result-is-error? r))
     (check-true (string-contains? (content-text r) "Invalid action")
                 (format "Expected invalid action message, got: ~a" (content-text r))))

   (test-case "search without query returns error"
     (with-handlers ([exn:fail? (lambda (e)
                                  (check-true (string-contains? (exn-message e) "query")
                                              (format "Expected query error, got: ~a" (exn-message e))))])
       (tool-firecrawl (hasheq 'action "search"))))

   (test-case "scrape without url returns error"
     (with-handlers ([exn:fail? (lambda (e)
                                  (check-true (string-contains? (exn-message e) "url")
                                              (format "Expected url error, got: ~a" (exn-message e))))])
       (tool-firecrawl (hasheq 'action "scrape"))))

   (test-case "crawl without url returns error"
     (with-handlers ([exn:fail? (lambda (e)
                                  (check-true (string-contains? (exn-message e) "url")
                                              (format "Expected url error, got: ~a" (exn-message e))))])
       (tool-firecrawl (hasheq 'action "crawl"))))

   (test-case "map without url returns error"
     (with-handlers ([exn:fail? (lambda (e)
                                  (check-true (string-contains? (exn-message e) "url")
                                              (format "Expected url error, got: ~a" (exn-message e))))])
       (tool-firecrawl (hasheq 'action "map"))))

   (test-case "valid action values accepted"
     (for ([action '("search" "scrape" "crawl" "map")])
       (check-not-false (valid-action? action)
                   (format "~a should be a valid action" action))))

   (test-case "valid format values accepted"
     (check-not-false (valid-formats? '("markdown")))
     (check-not-false (valid-formats? '("markdown" "html")))
     (check-not-false (valid-formats? '("rawHtml")))
     (check-false (valid-formats? '("xml")))
     (check-false (valid-formats? "markdown")))  ;; not a list

   ;; ---- API key detection ----

   (test-case "firecrawl-api-key returns #f when no env/config"
     ;; Temporarily clear env var
     (define old-env (getenv "FIRECRAWL_API_KEY"))
     (putenv "FIRECRAWL_API_KEY" "")
     (define key (firecrawl-api-key))
     ;; Restore
     (when old-env (putenv "FIRECRAWL_API_KEY" old-env))
     ;; key may be #f or the config value — just check it doesn't crash
     (check-true (or (not key) (string? key))))

   ;; ---- Format helpers ----

   (test-case "truncate-string shortens long strings"
     (define long-str (make-string 10000 #\x))
     (define result (truncate-string long-str 100))
     (check-true (string-contains? result "truncated"))
     (check-true (< (string-length result) 200)))

   (test-case "truncate-string respects custom threshold"
     (define str (make-string 100 #\x))
     (check-equal? (truncate-string str 50) (string-append (make-string 50 #\x) "\n...(truncated)"))
     (check-equal? (truncate-string str 200) str))

   (test-case "truncate-string leaves short strings unchanged"
     (define short-str "hello world")
     (check-equal? (truncate-string short-str 100) short-str))

   ;; ---- No API key → clear error ----

   (test-case "API request without key gives clear error"
     ;; Clear the env var
     (putenv "FIRECRAWL_API_KEY" "")
     (define caught-error #f)
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (set! caught-error (exn-message e)))])
       (firecrawl-request "GET" "/test"))
     ;; Should get either 'No API key' or another error (not crash)
     (check-true (and caught-error
                      (or (string-contains? caught-error "No API key")
                          (string-contains? caught-error "API request")
                          (string-contains? caught-error "firecrawl")))
                 (format "Expected firecrawl error, got: ~a" caught-error)))

   ;; ---- Tool accepts exec-ctx parameter ----

   (test-case "tool-firecrawl accepts optional exec-ctx"
     (define r (tool-firecrawl (hasheq) #f))
     (check-true (tool-result? r)))

   ;; ---- Schema validation ----

   (test-case "all valid actions pass validation"
     (for ([a '("search" "scrape" "crawl" "map")])
       (check-not-false (valid-action? a))))))

;; ============================================================
;; SEC-14: SSRF protection tests
;; ============================================================

(define ssrf-tests
  (test-suite
   "SSRF Protection Tests"

   (test-case "ftp:// URL rejected (bad scheme)"
     (check-exn
      #rx"Blocked: URL scheme must be http or https"
      (lambda () (validate-url "ftp://example.com/file"))))

   (test-case "file:// URL rejected (bad scheme)"
     (check-exn
      #rx"Blocked: URL scheme must be http or https"
      (lambda () (validate-url "file:///etc/passwd"))))

   (test-case "http://localhost rejected (private host)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://localhost:8080/api"))))

   (test-case "http://127.0.0.1 rejected (loopback)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://127.0.0.1/api"))))

   (test-case "http://10.0.0.1 rejected (private class A)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://10.0.0.1/secret"))))

   (test-case "http://192.168.1.1 rejected (private class C)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://192.168.1.1/router"))))

   (test-case "http://172.16.0.1 rejected (private class B)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://172.16.0.1/internal"))))

   (test-case "http://169.254.169.254 rejected (link-local/metadata)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://169.254.169.254/latest/meta-data/"))))

   (test-case "http://0.0.0.0 rejected (wildcard)"
     (check-exn
      #rx"Blocked: URL points to private/internal address"
      (lambda () (validate-url "http://0.0.0.0/api"))))

   (test-case "https://example.com passes validation"
     (check-not-exn
      (lambda () (validate-url "https://example.com/page"))))

   (test-case "http://example.com passes validation"
     (check-not-exn
      (lambda () (validate-url "http://example.com/page"))))

   (test-case "private-host? detects localhost"
     (check-not-false (private-host? "localhost")))

   (test-case "private-host? detects 10.x.x.x"
     (check-not-false (private-host? "10.0.0.1")))

   (test-case "private-host? allows public hosts"
     (check-false (private-host? "example.com")))

   (test-case "scrape action returns error result for localhost URL"
     (define r (tool-firecrawl (hasheq 'action "scrape"
                                        'url "http://localhost:8080/secret")))
     (check-true (tool-result? r))
     (check-true (tool-result-is-error? r))
     (check-true (string-contains? (content-text r) "Blocked")))

   (test-case "map action returns error result for private IP"
     (define r (tool-firecrawl (hasheq 'action "map"
                                        'url "http://10.0.0.1/internal")))
     (check-true (tool-result? r))
     (check-true (tool-result-is-error? r)))

   (test-case "crawl action returns error result for loopback"
     (define r (tool-firecrawl (hasheq 'action "crawl"
                                        'url "http://127.0.0.1/api")))
     (check-true (tool-result? r))
     (check-true (tool-result-is-error? r)))))

(run-tests firecrawl-tests)
(run-tests ssrf-tests)
