#lang racket

;; tests/test-settings-config-validation.rkt — v0.14.4 Wave 0
;;
;; Verifies config-parse-error detects broken JSON in config files.

(require rackunit
         racket/port
         "../runtime/settings.rkt")

;; ============================================================
;; config-parse-error tests
;; ============================================================

(test-case "config-parse-error returns #f for missing file"
  (define nonexistent "/tmp/q-test-config-nonexistent-xyz.json")
  (check-false (config-parse-error nonexistent)))

(test-case "config-parse-error returns #f for valid JSON object"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file tmp (lambda () (display "{\"providers\":{}}")) #:exists 'replace)
  (check-false (config-parse-error tmp))
  (delete-file tmp))

(test-case "config-parse-error returns message for invalid JSON"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file tmp (lambda () (display "{\"bad json")) #:exists 'replace)
  (define err (config-parse-error tmp))
  (check-not-false err)
  (check-true (string? err))
  (check-true (or (string-contains? err "expected")
                  (string-contains? err "Unexpected")
                  (string-contains? err "end of file")
                  (string-contains? err "unterminated")
                  (string-contains? err "read-json"))
              (format "Error message should mention JSON parse issue: ~a" err))
  (delete-file tmp))

(test-case "config-parse-error returns message for non-object top level"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file tmp (lambda () (display "[1,2,3]")) #:exists 'replace)
  (define err (config-parse-error tmp))
  (check-not-false err)
  (check-true (string-contains? err "not a JSON object") (format "Should mention non-object: ~a" err))
  (delete-file tmp))

(test-case "config-parse-error returns #f for valid nested config"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file
   tmp
   (lambda ()
     (display
      "{\"timeouts\":{\"models\":{\"glm-5.1\":{\"request\":900}}},\"providers\":{\"openai-compatible\":{\"max-tokens\":16384}}}"))
   #:exists 'replace)
  (check-false (config-parse-error tmp))
  (delete-file tmp))

;; ============================================================
;; F3: Config shape validation (v0.21.5)
;; ============================================================

(test-case "F3: load-settings with non-object config file returns empty merged"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file tmp (lambda () (display "[1,2,3]")) #:exists 'replace)
  (define settings (load-settings "/tmp" #:config-path tmp))
  (check-true (hash? (q-settings-merged settings)))
  (delete-file tmp))

(test-case "F3: load-settings with scalar config file returns empty merged"
  (define tmp (make-temporary-file "q-test-config-~a.json"))
  (with-output-to-file tmp (lambda () (display "\"hello\"")) #:exists 'replace)
  (define settings (load-settings "/tmp" #:config-path tmp))
  (check-true (hash? (q-settings-merged settings)))
  (delete-file tmp))

(test-case "F3: deep-merge-hash with non-hash left returns right"
  (check-equal? (deep-merge-hash "not-a-hash" (hash 'a 1)) (hash 'a 1)))

(test-case "F3: deep-merge-hash with non-hash right returns left"
  (check-equal? (deep-merge-hash (hash 'a 1) "not-a-hash") (hash 'a 1)))

(test-case "F3: deep-merge-hash with both non-hash returns empty"
  (check-equal? (deep-merge-hash 42 "string") (hash)))
