#lang racket

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; BOUNDARY: integration

;; tests/test-gsd-planning-edit-limit.rkt — Dynamic edit limit tests
;;
;; Tests for the edit limit (2000 default = SAFE-MAX-OLD-TEXT-LEN).
;; Uses box-based current-max-old-text-len (not parameter) for thread safety.

(require rackunit
         racket/file
         racket/string
         "../tools/builtins/edit.rkt"
         (only-in "../tools/tool.rkt" tool-result-is-error?))

;; ============================================================
;; Box-based state tests
;; ============================================================

(test-case "gsd-planning-edit-limit: current-max-old-text-len defaults to 2000"
  (check-equal? (current-max-old-text-len) 2000))

(test-case "current-max-old-text-len can be raised and restored"
  (define saved (current-max-old-text-len))
  (set-current-max-old-text-len! 2000)
  (check-equal? (current-max-old-text-len) 2000)
  (set-current-max-old-text-len! saved)
  (check-equal? (current-max-old-text-len) 2000))

(test-case "current-max-old-text-len persists across threads"
  (define saved (current-max-old-text-len))
  (set-current-max-old-text-len! 2000)
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-max-old-text-len))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) 2000)
  (set-current-max-old-text-len! saved))

;; ============================================================
;; Edit tool respects dynamic limit
;; ============================================================

(define (make-temp-file content)
  (define dir (make-temporary-file "edit-limit-test-~a" 'directory))
  (define f (build-path dir "test.txt"))
  (display-to-file content f #:exists 'replace)
  f)

(define (safe-old-text len)
  (apply string-append
         (for/list ([i (in-range len)])
           (string (integer->char (+ 65 (modulo i 26)))))))

(define (cleanup-path p)
  (when (file-exists? p)
    (delete-file p))
  (define dir (path-only p))
  (when (and dir (directory-exists? dir))
    (delete-directory/files dir)))

(test-case "edit rejects old-text > 2000 at default limit"
  (define f (make-temp-file (make-string 2100 #\x)))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text (make-string 2001 #\x) 'new-text "new")))
    (check-true (tool-result-is-error? result))
    (cleanup-path f)))

(test-case "edit accepts old-text ≤ 2000 at default limit"
  (define old (safe-old-text 2000))
  (define f (make-temp-file (string-append "prefix" old "suffix")))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result (tool-edit (hasheq 'path (path->string f) 'old-text old 'new-text "REPLACED")))
    (check-false (tool-result-is-error? result))
    (cleanup-path f)))

(test-case "edit accepts old-text up to 2000 at default limit"
  (define long-text (safe-old-text 1999))
  (define f (make-temp-file (string-append "prefix" long-text "suffix")))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text long-text 'new-text "REPLACED")))
    (check-false (tool-result-is-error? result))
    (cleanup-path f)))

(test-case "edit rejects old-text > 2000 at default limit"
  (define f (make-temp-file (safe-old-text 2001)))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-path f)
                               (raise e))])
    (define result
      (tool-edit (hasheq 'path (path->string f) 'old-text (safe-old-text 2001) 'new-text "new")))
    (check-true (tool-result-is-error? result))
    (cleanup-path f)))
