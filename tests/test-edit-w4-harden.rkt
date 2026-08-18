#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

;; W4: Filesystem hardening tests
;;
;; Coverage:
;;   - Non-UTF-8 encoding rejection (byte-level validation, NOT file->string)
;;   - Path identity hardening (double-check after resolution)
;;   - Backup collision resistance (timestamp + random suffix)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         (only-in "../tools/builtins/edit.rkt"
                  tool-edit
                  current-fuzzy-edit-enabled?
                  current-edit-before-replace-hook
                  current-edit-before-final-replace-hook)
         (only-in "../tools/tool.rkt" tool-result-is-error? tool-result-content tool-result-details)
         (only-in "../tools/builtins/builtin-helpers.rkt" check-utf8-file? validate-utf8-bytes)
         (only-in "../tools/exec-context.rkt" make-exec-context))

;; --------------------------------------------------
;; Helpers
;; --------------------------------------------------

(define (write-bytes-to-file bytes path)
  (call-with-output-file path #:exists 'replace (lambda (out) (write-bytes bytes out))))

(define (details-text result)
  (format "~a" (tool-result-content result)))

;; --------------------------------------------------
;; W4.1: Non-UTF-8 encoding check
;; --------------------------------------------------

(define utf8-suite
  (test-suite "W4.1: Non-UTF-8 encoding detection"

    (test-case "valid ASCII file passes UTF-8 check"
      (define p (make-temporary-file "q-w4-ascii-~a"))
      (dynamic-wind void
                    (lambda ()
                      (display-to-file "hello world" p #:exists 'replace)
                      (check-equal? (check-utf8-file? p) #t))
                    (lambda () (delete-file p))))

    (test-case "valid UTF-8 with multi-byte chars passes"
      (define p (make-temporary-file "q-w4-utf8-ok-~a"))
      (dynamic-wind void
                    (lambda ()
                      ;; U+2014 (em dash) = E2 80 94, U+00E9 (e-acute) = C3 A9
                      (write-bytes-to-file #"hello \xe2\x80\x94 world \xc3\xa9" p)
                      (check-equal? (check-utf8-file? p) #t))
                    (lambda () (delete-file p))))

    (test-case "valid UTF-8 with emoji (4-byte sequence) passes"
      (define p (make-temporary-file "q-w4-emoji-ok-~a"))
      (dynamic-wind void
                    (lambda ()
                      (write-bytes-to-file #"party \xf0\x9f\x8e\x89 time" p)
                      (check-equal? (check-utf8-file? p) #t))
                    (lambda () (delete-file p))))

    (test-case "overlong three-byte encoding returns a descriptive error"
      (define result (validate-utf8-bytes #"\xe0\x80\x80"))
      (check-true (string? result))
      (check-true (string-contains? result "overlong")))

    (test-case "UTF-8 surrogate encoding returns a descriptive error"
      (define result (validate-utf8-bytes #"\xed\xa0\x80"))
      (check-true (string? result))
      (check-true (string-contains? result "surrogate")))

    (test-case "edit tool rejects non-UTF-8 file with descriptive error"
      (define p (make-temporary-file "q-w4-0xff-~a"))
      (dynamic-wind
       void
       (lambda ()
         (write-bytes-to-file #"hello \xff world" p)
         (define result
           (tool-edit (hasheq 'path (path->string p) 'old-text "hello" 'new-text "goodbye") #f))
         (check-true (tool-result-is-error? result) "should reject file with 0xFF byte")
         (check-true (string-contains? (details-text result) "UTF-8") "error should mention UTF-8"))
       (lambda () (delete-file p))))

    (test-case "edit tool rejects file with invalid continuation byte"
      (define p (make-temporary-file "q-w4-bad-cont-~a"))
      (dynamic-wind void
                    (lambda ()
                      (write-bytes-to-file #"abc\xc3\x00xyz" p)
                      (define result
                        (tool-edit (hasheq 'path (path->string p) 'old-text "abc" 'new-text "ABC")
                                   #f))
                      (check-true (tool-result-is-error? result))
                      (check-true (string-contains? (details-text result) "UTF-8")))
                    (lambda () (delete-file p))))

    (test-case "edit tool rejects file with truncated multi-byte sequence"
      (define p (make-temporary-file "q-w4-trunc-~a"))
      (dynamic-wind void
                    (lambda ()
                      (write-bytes-to-file #"lead \xe0\x80 only" p)
                      (define result
                        (tool-edit (hasheq 'path (path->string p) 'old-text "lead" 'new-text "new")
                                   #f))
                      (check-true (tool-result-is-error? result))
                      (check-true (string-contains? (details-text result) "UTF-8")))
                    (lambda () (delete-file p))))

    (test-case "edit tool accepts valid UTF-8 file"
      (define p (make-temporary-file "q-w4-valid-txt-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file "some text to edit" p #:exists 'replace)
         (define result
           (tool-edit (hasheq 'path (path->string p) 'old-text "text" 'new-text "content") #f))
         (check-false (tool-result-is-error? result) "valid UTF-8 edit should succeed")
         (check-equal? (file->string p) "some content to edit" "file content should be updated"))
       (lambda () (delete-file p))))

    (test-case "edit tool preserves file unchanged on UTF-8 rejection"
      (define p (make-temporary-file "q-w4-no-mutate-~a"))
      (define original-bytes #"before \xff corrupt")
      (dynamic-wind
       void
       (lambda ()
         (write-bytes-to-file original-bytes p)
         (define result
           (tool-edit (hasheq 'path (path->string p) 'old-text "before" 'new-text "after") #f))
         (check-true (tool-result-is-error? result))
         (check-equal? (file->bytes p) original-bytes "file content must not change on failed edit"))
       (lambda () (delete-file p))))))

;; --------------------------------------------------
;; W4.2: Path identity hardening
;; --------------------------------------------------

(define identity-suite
  (test-suite "W4.2: Path identity hardening"

    (test-case "normal direct path succeeds identity check"
      (define p (make-temporary-file "q-w4-identity-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file "hello world" p #:exists 'replace)
         (define result
           (tool-edit (hasheq 'path (path->string p) 'old-text "hello" 'new-text "goodbye") #f))
         (check-false (tool-result-is-error? result) "edit via direct path should succeed")
         (check-equal? (file->string p) "goodbye world"))
       (lambda () (delete-file p))))

    (test-case "relative path resolves under execution working directory"
      (define dir (make-temporary-file "q-w4-relative-~a" 'directory))
      (define p (build-path dir "target.txt"))
      (dynamic-wind void
                    (lambda ()
                      (display-to-file "before" p #:exists 'replace)
                      (define result
                        (tool-edit (hasheq 'path "target.txt" 'old-text "before" 'new-text "after")
                                   (make-exec-context #:working-directory dir)))
                      (check-false (tool-result-is-error? result))
                      (check-equal? (file->string p) "after"))
                    (lambda () (delete-directory/files dir))))

    (test-case "edit via symlink resolves and succeeds"
      (define dir (make-temporary-file "q-w4-symlink-~a" 'directory))
      (define real-file (build-path dir "target.txt"))
      (define link-path (build-path dir "link.txt"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file "original" real-file #:exists 'replace)
         (make-file-or-directory-link real-file link-path)
         (define result
           (tool-edit
            (hasheq 'path (path->string link-path) 'old-text "original" 'new-text "modified")
            #f))
         (check-false (tool-result-is-error? result) "edit via symlink should succeed")
         (check-equal? (file->string real-file) "modified" "target file should be modified"))
       (lambda () (delete-directory/files dir))))))

;; --------------------------------------------------
;; W4.3: Backup collision resistance
;; --------------------------------------------------

(define backup-suite
  (test-suite "W4.3: Backup collision resistance"

    (test-case "consecutive edits produce uniquely-named backups"
      (define p (make-temporary-file "q-w4-backup-unique-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file "v1" p #:exists 'replace)
         (define r1 (tool-edit (hasheq 'path (path->string p) 'old-text "v1" 'new-text "v2") #f))
         (check-false (tool-result-is-error? r1) "first edit should succeed")
         (define b1 (hash-ref (tool-result-details r1) 'backup ""))
         (display-to-file "v2" p #:exists 'replace)
         (define r2 (tool-edit (hasheq 'path (path->string p) 'old-text "v2" 'new-text "v3") #f))
         (check-false (tool-result-is-error? r2) "second edit should succeed")
         (define b2 (hash-ref (tool-result-details r2) 'backup ""))
         (check-not-equal? b1 b2 "backup filenames must differ between edits"))
       (lambda () (delete-file p))))

    (test-case "backup filename contains timestamp and exclusive unique component"
      (define p (make-temporary-file "q-w4-backup-format-~a"))
      (dynamic-wind
       void
       (lambda ()
         (display-to-file "content" p #:exists 'replace)
         (define result
           (tool-edit (hasheq 'path (path->string p) 'old-text "content" 'new-text "updated") #f))
         (check-false (tool-result-is-error? result))
         (define backup-path (hash-ref (tool-result-details result) 'backup ""))
         (check-true (file-exists? backup-path) (format "backup ~a should exist" backup-path))
         (check-true (regexp-match? #px"_[0-9]+_" backup-path)
                     (format "backup ~a should contain an exclusive unique token" backup-path)))
       (lambda () (delete-file p))))))

;; --------------------------------------------------
;; Run all W4 tests
;; --------------------------------------------------

(define w4-suite
  (test-suite "W4: Filesystem hardening"
    utf8-suite
    identity-suite
    backup-suite))

(module+ test
  (run-tests w4-suite))

(module+ main
  (run-tests w4-suite))
