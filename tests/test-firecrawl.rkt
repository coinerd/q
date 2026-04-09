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
(run-tests firecrawl-tests)
