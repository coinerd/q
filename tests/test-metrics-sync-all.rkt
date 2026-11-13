#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration
;; tests/test-metrics-sync-all.rkt — Tests for metrics.rkt --sync-all

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/system)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define script (build-path project-root "scripts" "metrics.rkt"))
(define live-readme (build-path project-root "README.md"))
(define racket-executable (find-executable-path "racket"))

(define (with-readme-copy proc)
  (define dir (make-temporary-file "metrics-sync-all-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define readme (build-path dir "README.md"))
                  (copy-file live-readme readme)
                  (proc readme))
                (lambda () (delete-directory/files dir))))

(define (sync-all readme)
  (parameterize ([current-directory project-root])
    (system*/exit-code racket-executable script "--sync-all" readme)))

(test-case "metrics.rkt --sync-all exits 0 without changing the live README"
  (define before (file->bytes live-readme))
  (with-readme-copy (lambda (readme)
                      (check-equal? (sync-all readme) 0)
                      (check-equal? (file->bytes live-readme) before))))

(test-case "temporary fixture is removed when the body fails"
  (define fixture-dir #f)
  (check-exn exn:fail?
             (lambda ()
               (with-readme-copy (lambda (readme)
                                   (set! fixture-dir (path-only readme))
                                   (error 'fixture "intentional failure")))))
  (check-not-false fixture-dir)
  (check-false (directory-exists? fixture-dir)))

(test-case "--sync-all fixes corrupted prose count"
  (with-readme-copy (lambda (readme)
                      (define original (file->string readme))
                      (define corrupted
                        (regexp-replace* #rx"Full test suite \\([0-9]+ files\\)"
                                         original
                                         "Full test suite (999 files)"))
                      (check-not-equal? corrupted original)
                      (check-true (string-contains? corrupted "Full test suite (999 files)"))
                      (display-to-file corrupted readme #:exists 'replace)
                      (check-equal? (sync-all readme) 0)
                      (define fixed (file->string readme))
                      (check-false (string-contains? fixed "Full test suite (999 files)"))
                      (check-not-false (regexp-match? #rx"Full test suite \\([0-9]+ files\\)"
                                                      fixed)))))

(test-case "--sync-all fixes corrupted table value"
  (with-readme-copy
   (lambda (readme)
     (define original (file->string readme))
     (define corrupted
       (regexp-replace* #rx"\\| Source lines \\| [0-9]+ \\|" original "| Source lines | 00000 |"))
     (check-not-equal? corrupted original)
     (check-true (string-contains? corrupted "| Source lines | 00000 |"))
     (display-to-file corrupted readme #:exists 'replace)
     (check-equal? (sync-all readme) 0)
     (define fixed (file->string readme))
     (check-false (string-contains? fixed "| Source lines | 00000 |")))))
