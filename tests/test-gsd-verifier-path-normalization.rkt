#lang racket/base
;; @covers extensions/gsd/plan-types-parser.rkt
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed fast  ;; @suite extensions
;; @boundary integration
;; tests/test-gsd-verifier-path-normalization.rkt — BUG-0025 regression suite
;;
;; Wave docs declare file targets with trailing bracket annotations, e.g.
;;   - File: q/tests/foo.rkt  [NEW]
;;   - File: `q/ui-core/bar.rkt` [NEW, design record]
;; Before the fix, clean-file-path kept the annotation in the parsed path
;; ("q/tests/foo.rkt  [NEW]"), so wave-file->git-relative never matched a
;; real file and delivery verification failed with "no wave target files
;; changed" despite a green delivery (live false failure: v1.00.17 W0).
;;
;; These tests pin the normalization at the delivery-verification level
;; (parse + files gate + failure diagnostics). Unit-level pins for the
;; parser live in test-gsd-plan-format-characterization.rkt.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-message
                  current-gsd-delivery-verify-command)
         (only-in "../extensions/gsd/plan-types.rkt" gsd-plan make-gsd-wave))

;; ============================================================
;; Fixture: temp git repo (mirrors test-gsd-delivery-verifier.rkt)
;; ============================================================

(define GIT (find-executable-path "git"))

(define (make-tmp-git-repo)
  (define base (make-temporary-file "pn-base-~a" 'directory))
  (make-directory* (build-path base ".planning"))
  (make-directory* (build-path base "q" "ui-core"))
  (make-directory* (build-path base "q" "tests"))
  (define (sh . args)
    (define exit
      (parameterize ([current-directory base])
        (apply system*/exit-code GIT args)))
    (unless (zero? exit)
      (error 'make-tmp-git-repo "command failed: ~a" (cons 'sh args))))
  (sh "init" "-q" ".")
  (sh "config" "user.email" "test@example.com")
  (sh "config" "user.name" "Test")
  (sh "checkout" "-q" "-b" "main")
  ;; baseline tracked wave-target file
  (call-with-output-file (build-path base "q" "ui-core" "preferences.rkt")
                         (lambda (out)
                           (display "#lang racket/base\n(provide foo)\n(define foo 1)\n" out))
                         #:exists 'truncate)
  (sh "add" "-A")
  (sh "commit" "-q" "-m" "baseline")
  base)

;; One-wave plan whose declared files are exactly `files`. STATE.md is left
;; without a wave/issue table: the branch gate then only requires a git repo
;; and does not force a feature/issue-<N> branch (matches the issue-less
;; campaign layout), so the files gate is what these tests exercise.
(define (load-plan* files)
  (gsd-plan (list (make-gsd-wave 0 "Wave Zero" "" files '() "true" (list "done"))) "" '() '()))

(define (verify base files)
  (parameterize ([current-gsd-delivery-verify-command "true"])
    (run-delivery-verification base (load-plan* files) 0)))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(define (path-normalization-suite)
  (test-suite "verifier-path-normalization"

    (test-case "annotated [NEW] file delivered as untracked passes delivery verification"
      ;; The pre-fix false failure: the declaration `q/tests/x.rkt  [NEW]`
      ;; parsed with the annotation attached, so an actually-delivered new
      ;; file looked like no delivery at all.
      (define base (make-tmp-git-repo))
      ;; delivery: new file on disk, untracked
      (call-with-output-file (build-path base "q" "tests" "annotated-new.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide x)\n(define x 1)\n" out))
                             #:exists 'truncate)
      (define result (verify base '("q/tests/annotated-new.rkt  [NEW]")))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (cleanup-tmp base))

    (test-case "combined backtick + annotation form works"
      ;; `path` [NEW, design record] — both backtick quoting and a
      ;; comma-carrying bracket annotation must be stripped.
      (define base (make-tmp-git-repo))
      (call-with-output-file
       (build-path base "q" "ui-core" "preferences.rkt")
       (lambda (out)
         (display "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n" out))
       #:exists 'truncate)
      (define result (verify base '("`q/ui-core/preferences.rkt` [NEW, design record]")))
      (check-true (delivery-verification-approved? result)
                  "backtick + annotation declaration must match the modified file")
      (cleanup-tmp base))

    (test-case "annotated declaration matches tracked file via git diff"
      (define base (make-tmp-git-repo))
      (call-with-output-file
       (build-path base "q" "ui-core" "preferences.rkt")
       (lambda (out)
         (display "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n" out))
       #:exists 'truncate)
      (define result (verify base '("q/ui-core/preferences.rkt [NEW]")))
      (check-true (delivery-verification-approved? result)
                  "annotated declaration must resolve to the changed tracked file")
      (cleanup-tmp base))

    (test-case "paths without annotations behave exactly as before"
      ;; Unannotated declarations keep the pre-fix semantics: a modified
      ;; tracked file passes...
      (define base (make-tmp-git-repo))
      (call-with-output-file
       (build-path base "q" "ui-core" "preferences.rkt")
       (lambda (out)
         (display "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n" out))
       #:exists 'truncate)
      (define result (verify base '("q/ui-core/preferences.rkt")))
      (check-true (delivery-verification-approved? result)
                  "unannotated modified file must still pass")
      (cleanup-tmp base))

    (test-case "unannotated unchanged file still fails closed"
      ;; ...and a non-delivery still fails, with the diagnostic naming the
      ;; computed git-relative path (not the raw annotated declaration).
      (define base (make-tmp-git-repo))
      (define result (verify base '("q/ui-core/preferences.rkt")))
      (check-false (delivery-verification-approved? result)
                   "no delivery must still fail without annotations")
      (cleanup-tmp base))

    (test-case "files-gate failure message shows computed git-relative paths"
      ;; Diagnosability requirement: the failure must show the normalized
      ;; git-relative path per declared file so annotation/prefix mismatches
      ;; are visible, and must not leak the raw `... [NEW]` string.
      (define base (make-tmp-git-repo))
      (define result (verify base '("q/ui-core/missing-new.rkt  [NEW]" "q/ui-core/preferences.rkt")))
      (check-false (delivery-verification-approved? result))
      (define msg (delivery-verification-message result))
      (check-true (string-contains? msg "q/ui-core/missing-new.rkt")
                  (format "message must contain the computed path; got: ~a" msg))
      (check-true (string-contains? msg "q/ui-core/preferences.rkt")
                  (format "message must contain the second computed path; got: ~a" msg))
      (check-false (string-contains? msg "[NEW]")
                   (format "message must show normalized paths, not raw declarations; got: ~a" msg))
      (cleanup-tmp base))))

(module+ test
  (void (run-tests (path-normalization-suite))))

(module+ main
  ;; Wave verify commands run this file directly via `racket <file>`; a
  ;; module+ test-only file would execute NOTHING on direct invocation
  ;; (vacuous green). Run the suite and propagate failures as the exit code.
  (exit (run-tests (path-normalization-suite))))
