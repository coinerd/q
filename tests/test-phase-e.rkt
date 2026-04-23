#lang racket/base

;; tests/test-phase-e.rkt — Tests for Phase E extensions

(require rackunit
         racket/file
         racket/string
         (only-in "../extensions/ext-package-manager.rkt" handle-ext-pkg)
         (only-in "../extensions/image-input.rkt"
                  handle-image-input
                  image->base64
                  extension->media-type)
         (only-in "../extensions/session-export.rkt" handle-session-export)
         "../tools/tool.rkt")

;; ============================================================
;; ext-package-manager tests
;; ============================================================

(test-case "handle-ext-pkg list returns success"
  (define result (handle-ext-pkg (hasheq 'action "list")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result)))

(test-case "handle-ext-pkg info non-existent returns error"
  (define result (handle-ext-pkg (hasheq 'action "info" 'name "nonexistent")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "handle-ext-pkg unknown action returns error"
  (define result (handle-ext-pkg (hasheq 'action "bogus")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; image-input tests
;; ============================================================

(test-case "image-input encode requires path"
  (with-handlers ([exn:fail? (lambda (e) (check-true (string-contains? (exn-message e) "path")))])
    (handle-image-input (hasheq 'action "encode"))
    (check-false "Should have raised" #t)))

(test-case "image-input file not found errors"
  (with-handlers ([exn:fail? (lambda (e)
                               (check-true (string-contains? (exn-message e) "not found")))])
    (handle-image-input (hasheq 'action "encode" 'path "/nonexistent.png"))
    (check-false "Should have raised" #t)))

(test-case "image->base64 encodes bytes"
  (define tmp (make-temporary-file "img-~a.png"))
  (call-with-output-file tmp (lambda (out) (display #"PNGFake" out)) #:exists 'replace)
  (define b64 (image->base64 tmp))
  (check-true (non-empty-string? b64))
  (delete-file tmp))

(test-case "extension->media-type detects PNG"
  (check-equal? (extension->media-type "test.png") "image/png"))

(test-case "extension->media-type detects JPEG"
  (check-equal? (extension->media-type "photo.jpg") "image/jpeg"))

(test-case "extension->media-type defaults to PNG"
  (check-equal? (extension->media-type "file.xyz") "image/png"))

(test-case "image-input unknown action returns error"
  (define result (handle-image-input (hasheq 'action "bogus" 'path "/tmp/x.png")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; session-export tests
;; ============================================================

(test-case "session-export requires session_path"
  (with-handlers ([exn:fail? (lambda (e)
                               (check-true (string-contains? (exn-message e) "session_path")))])
    (handle-session-export (hasheq 'format "markdown"))
    (check-false "Should have raised" #t)))

(test-case "session-export file not found errors"
  (with-handlers ([exn:fail? (lambda (e)
                               (check-true (string-contains? (exn-message e) "not found")))])
    (handle-session-export (hasheq 'session_path "/nonexistent.jsonl"))
    (check-false "Should have raised" #t)))

(test-case "session-export markdown export works"
  (define tmp (make-temporary-file "session-~a.jsonl"))
  (call-with-output-file tmp
                         (lambda (out)
                           (displayln "{\"role\":\"user\",\"content\":\"hello\"}" out)
                           (displayln "{\"role\":\"assistant\",\"content\":\"world\"}" out))
                         #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "markdown")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "User"))
  (check-true (string-contains? text "hello"))
  (delete-file tmp))

(test-case "session-export json export works"
  (define tmp (make-temporary-file "session-~a.jsonl"))
  (call-with-output-file tmp
                         (lambda (out) (displayln "{\"role\":\"user\",\"content\":\"test\"}" out))
                         #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "json")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (delete-file tmp))

(test-case "session-export html export works"
  (define tmp (make-temporary-file "session-~a.jsonl"))
  (call-with-output-file tmp
                         (lambda (out)
                           (displayln "{\"role\":\"assistant\",\"content\":\"response\"}" out))
                         #:exists 'replace)
  (define result (handle-session-export (hasheq 'session_path (path->string tmp) 'format "html")))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (define text (hash-ref (car (tool-result-content result)) 'text ""))
  (check-true (string-contains? text "<html>"))
  (delete-file tmp))

(test-case "session-export writes to output file"
  (define tmp (make-temporary-file "session-~a.jsonl"))
  (define out-path (make-temporary-file "out-~a.md"))
  (delete-file out-path)
  (call-with-output-file tmp
                         (lambda (out) (displayln "{\"role\":\"user\",\"content\":\"hi\"}" out))
                         #:exists 'replace)
  (define result
    (handle-session-export (hasheq 'session_path
                                   (path->string tmp)
                                   'format
                                   "markdown"
                                   'output_path
                                   (path->string out-path))))
  (check-pred tool-result? result)
  (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text "") "Exported to"))
  (delete-file tmp)
  (when (file-exists? out-path)
    (delete-file out-path)))
