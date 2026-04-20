#lang racket/base

;; tests/test-metrics-sync-all.rkt — Tests for metrics.rkt --sync-all

(require rackunit
         racket/file
         racket/string
         racket/system
         racket/path)

(define project-root
  (simplify-path (build-path (or (path-only (resolved-module-path-name
                                              (variable-reference->resolved-module-path
                                               (#%variable-reference))))
                                 ".")
                             "..")))

(define script (build-path project-root "scripts" "metrics.rkt"))

(test-case "metrics.rkt --sync-all exits 0"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --sync-all" project-root script)))
  (check-equal? exit-code 0))

(test-case "--sync-all fixes corrupted prose count"
  (define readme (build-path project-root "README.md"))
  (define backup (file->string readme))
  ;; Corrupt prose count to something obviously wrong
  (define corrupted (regexp-replace* #rx"Full test suite \\([0-9]+ files\\)"
                                     backup
                                     "Full test suite (999 files)"))
  (call-with-output-file readme (λ (out) (display corrupted out)) #:exists 'replace)
  ;; Run sync-all
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --sync-all" project-root script)))
  ;; Verify: should NOT still say 999
  (define fixed (file->string readme))
  (check-false (string-contains? fixed "Full test suite (999 files)"))
  ;; Should have a real number
  (check-not-false (regexp-match? #rx"Full test suite \\([0-9]+ files\\)" fixed))
  ;; Restore backup regardless
  (call-with-output-file readme (λ (out) (display backup out)) #:exists 'replace))

(test-case "--sync-all fixes corrupted table value"
  (define readme (build-path project-root "README.md"))
  (define backup (file->string readme))
  ;; Corrupt table
  (define corrupted (regexp-replace* #rx"\\| Source lines \\| [0-9]+ \\|"
                                     backup
                                     "| Source lines | 00000 |"))
  (call-with-output-file readme (λ (out) (display corrupted out)) #:exists 'replace)
  ;; Run sync-all
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --sync-all" project-root script)))
  ;; Verify fixed (should have a real number, not 00000)
  (define fixed (file->string readme))
  (check-false (string-contains? fixed "| Source lines | 00000 |"))
  ;; Restore
  (call-with-output-file readme (λ (out) (display backup out)) #:exists 'replace))
