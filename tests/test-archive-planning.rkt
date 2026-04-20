#lang racket/base

;; tests/test-archive-planning.rkt — Tests for scripts/archive-planning.rkt
;;
;; Uses temp directory fixtures instead of the real .planning/ (which is
;; gitignored and absent on CI).

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/port
         racket/system)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define script (build-path project-root "scripts" "archive-planning.rkt"))

;; --- Temp fixture helpers ---

(define (make-planning-fixture files)
  ;; Create a temp dir with given files. files = list of (relative-path . content)
  (define tmp (make-temporary-file "archive-planning-test-~a" 'directory))
  (for ([f (in-list files)])
    (define path (build-path tmp (car f)))
    (make-directory* (path-only path))
    (display-to-file (cdr f) path #:exists 'replace))
  tmp)

(define (cleanup-fixture tmp)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp)))

(define (run-script tmp . extra-args)
  (define args-str (string-join (cons (format "--planning-dir=~a" tmp) extra-args)))
  (define cmd (format "racket ~a ~a 2>&1" script args-str))
  (define out (with-output-to-string (λ () (system cmd))))
  (define ec (system/exit-code (format "racket ~a ~a > /dev/null 2>&1" script args-str)))
  (values out ec))

;; --- Tests ---

(test-case "archive-planning.rkt exists"
  (check-true (file-exists? script)))

(test-case "exits 1 when planning dir missing"
  (define-values (out ec) (run-script "/tmp/no-such-dir-archive-test"))
  (check-equal? ec 1)
  (check-true (string-contains? out "not found")))

(test-case "--dry-run on empty planning dir"
  (define tmp (make-planning-fixture '()))
  (dynamic-wind void
                (λ ()
                  (define-values (out ec) (run-script tmp "--dry-run"))
                  (check-equal? ec 0)
                  (check-true (string-contains? out "No archive candidates")))
                (λ () (cleanup-fixture tmp))))

(test-case "--dry-run detects old milestone files"
  (define tmp
    (make-planning-fixture '(("PLAN.md" . "# Plan\nstuff") ("STATE.md" . "# State\nv0.12.1 stuff")
                                                           ("V0113_PLAN.md" . "old plan")
                                                           ("V0113_REVIEW.md" . "old review")
                                                           ("BUG_REPORT.md" . "no version prefix"))))
  (dynamic-wind void
                (λ ()
                  (define-values (out ec) (run-script tmp "--dry-run"))
                  (check-equal? ec 0)
                  (check-true (string-contains? out "archive candidates"))
                  ;; Should detect V0113 files
                  (check-true (string-contains? out "V0113_PLAN.md"))
                  ;; Should NOT archive BUG_REPORT.md (no version prefix)
                  (check-false (string-contains? out "BUG_REPORT"))
                  ;; Should mention v0113 group
                  (check-true (regexp-match? #rx"\\[v0113\\]" out)))
                (λ () (cleanup-fixture tmp))))

(test-case "protects current milestone files"
  (define tmp
    (make-planning-fixture '(("STATE.md" . "# State: v0.12.1\nactive")
                             ("V0121_WAVE0.md" . "current wave")
                             ("V0113_PLAN.md" . "old"))))
  (dynamic-wind void
                (λ ()
                  (define-values (out ec) (run-script tmp "--dry-run"))
                  (check-equal? ec 0)
                  ;; V0121 is current (from STATE.md) — should NOT be candidate
                  (check-false (string-contains? out "V0121_WAVE0"))
                  ;; V0113 is old — should be candidate
                  (check-true (string-contains? out "V0113_PLAN")))
                (λ () (cleanup-fixture tmp))))

(test-case "actually moves files (non-dry-run)"
  (define tmp
    (make-planning-fixture '(("STATE.md" . "# State: v0.12.1\nactive") ("V0113_PLAN.md" .
                                                                                        "old plan"))))
  (dynamic-wind void
                (λ ()
                  (define-values (out ec) (run-script tmp))
                  (check-equal? ec 0)
                  (check-true (string-contains? out "1 files moved"))
                  ;; File should now be in archive/
                  (check-true (file-exists? (build-path tmp "archive" "v0113" "V0113_PLAN.md")))
                  ;; Original should be gone
                  (check-false (file-exists? (build-path tmp "V0113_PLAN.md"))))
                (λ () (cleanup-fixture tmp))))
