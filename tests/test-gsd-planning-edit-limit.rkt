#lang racket

;; tests/test-gsd-planning-edit-limit.rkt — Dynamic edit limit tests
;;
;; Tests for v0.20.2 Wave 1: Dynamic edit limit (500 default, 1200 during /go).
;; Uses box-based current-max-old-text-len (not parameter) for thread safety.

(require rackunit
         racket/file
         racket/string
         "../tools/builtins/edit.rkt"
         (only-in "../tools/tool.rkt" tool-result-is-error?))

;; ============================================================
;; Box-based state tests
;; ============================================================

(test-case "current-max-old-text-len defaults to 500"
  (check-equal? (current-max-old-text-len) 500))

(test-case "current-max-old-text-len can be raised and restored"
  (define saved (current-max-old-text-len))
  (set-current-max-old-text-len! 1200)
  (check-equal? (current-max-old-text-len) 1200)
  (set-current-max-old-text-len! saved)
  (check-equal? (current-max-old-text-len) 500))

(test-case "current-max-old-text-len persists across threads"
  (define saved (current-max-old-text-len))
  (set-current-max-old-text-len! 1200)
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-max-old-text-len))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) 1200)
  (set-current-max-old-text-len! saved))

;; ============================================================
;; Edit tool respects dynamic limit
;; ============================================================

(define (make-temp-file content)
  (define dir (make-temporary-file "edit-limit-test-~a" 'directory))
  (define f (build-path dir "test.txt"))
  (display-to-file content f #:exists 'replace)
  f)

(define (cleanup-path p)
  (when (file-exists? p)
    (delete-file p))
  (define dir (path-only p))
  (when (and dir (directory-exists? dir))
    (delete-directory/files dir)))

(test-case "edit rejects old-text > 500 at default limit"
  (define f (make-temp-file (make-string 600 #\x)))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text (make-string 501 #\x) 'new-text "new")))
    (check-true (tool-result-is-error? result))
    (cleanup-path f)))

(test-case "edit accepts old-text ≤ 500 at default limit"
  (define f (make-temp-file (string-append "prefix" (make-string 500 #\x) "suffix")))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result
      (tool-edit
       (hasheq 'path (path->string f) 'old-text (make-string 500 #\x) 'new-text "REPLACED")))
    (check-false (tool-result-is-error? result))
    (cleanup-path f)))

(test-case "edit accepts old-text up to 1200 at raised limit"
  (define long-text (make-string 1000 #\x))
  (define f (make-temp-file (string-append "prefix" long-text "suffix")))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define saved (current-max-old-text-len))
    (set-current-max-old-text-len! 1200)
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text long-text 'new-text "REPLACED")))
    (check-false (tool-result-is-error? result))
    (set-current-max-old-text-len! saved)
    (cleanup-path f)))

(test-case "edit rejects old-text > 1200 at raised limit"
  (define f (make-temp-file (make-string 1300 #\x)))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define saved (current-max-old-text-len))
    (set-current-max-old-text-len! 1200)
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text (make-string 1201 #\x) 'new-text "new")))
    (check-true (tool-result-is-error? result))
    (set-current-max-old-text-len! saved)
    (cleanup-path f)))
