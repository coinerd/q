#lang racket/base
;; Trusted datum boundary regressions: hash-only reads, gate-red binding
;; drafts with honest PENDING placeholders, and frozen snapshot provenance
;; verified through the trusted plan-snapshot module (no filename guessing,
;; no wave-index/mtime shortcuts).
(require rackunit
         racket/file
         racket/format
         racket/path
         racket/string
         (only-in "../extensions/gsd/plan-snapshot.rkt" make-plan-snapshot! snapshot-dir)
         "../scripts/gsd-binding-data.rkt")

(define campaign-id (make-string 64 #\a))
(define other-campaign-id (make-string 64 #\b))

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

(define (plan-tree #:wave-text [wave-text #f])
  (define dir (make-temporary-file "binding-data-~a" 'directory))
  (write-text! (build-path dir ".planning" "PLAN.md")
               (string-append "# Plan\n\n"
                              "- [Inbox] W1: Fixture -> waves/W1-fixture.md\n"
                              "- [Inbox] W2: Later -> waves/W2-later.md\n"))
  (write-text! (build-path dir ".planning" "waves" "W1-fixture.md")
               (or wave-text
                   (string-append
                    "# W1: Fixture wave\n\n"
                    "Branch: `campaign/v9.9.9-w1`; GitHub issue "
                    "[#9686](https://github.com/owner/repo/issues/9686), milestone #895.\n\n"
                    "## Files\n\n"
                    "- File: `q/docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd`\n"
                    "- File: `q/docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd`\n"
                    "- File: `q/docs/reports/gsd-wave-validation/v9.9.9-w1.rktd`\n")))
  (write-text! (build-path dir ".planning" "waves" "W2-later.md") "# W2: Later\n\nbody\n")
  dir)

(define (bind-snapshot! dir)
  (make-plan-snapshot! dir
                       campaign-id
                       (file->string (build-path dir ".planning" "PLAN.md"))
                       #:plan-id campaign-id))

(define draft-request
  (hasheq 'plan-id
          campaign-id
          'wave
          "W1"
          'merge-sha
          (make-string 40 #\b)
          'delivery-pr
          42
          'delivery-head-sha
          (make-string 40 #\c)
          'wave-branch
          "campaign/v9.9.9-w1"
          'merged-at
          "2026-09-14T16:23:06Z"
          'branch
          "binding/draft"
          'milestone
          895
          'issue
          9686
          'status
          "pending-review"
          'required-pr-checks
          '("lint" "test (0)" "workflows (0)")))

(module+ test
  (test-case "only a single hash datum is accepted"
    (define p (make-temporary-file))
    (dynamic-wind
     void
     (lambda ()
       (for ([text '("((merge-sha . \"fake\"))" "#hash() #hash()" "#hash() garbage" "(\"lint\")")])
         (display-to-file text p #:exists 'truncate)
         (check-exn exn:fail? (lambda () (read-hash-datum p))))
       (display-to-file "#hash((wave . \"W1\"))" p #:exists 'truncate)
       (check-equal? (hash-ref (read-hash-datum p) 'wave) "W1"))
     (lambda () (delete-file p))))

  (test-case "read-any-datum accepts the policy list shape and rejects noise"
    (define p (make-temporary-file))
    (dynamic-wind void
                  (lambda ()
                    (display-to-file "(\"lint\" \"test (0)\")" p #:exists 'truncate)
                    (check-equal? (read-any-datum p) '("lint" "test (0)"))
                    (display-to-file "" p #:exists 'truncate)
                    (check-exn exn:fail? (lambda () (read-any-datum p)))
                    (display-to-file "() ()" p #:exists 'truncate)
                    (check-exn exn:fail? (lambda () (read-any-datum p))))
                  (lambda () (delete-file p))))

  (test-case "draft trio cannot fabricate independent review or validation"
    (define-values (e r v) (binding-draft draft-request))
    (check-equal? (hash-ref e 'schema-version) 2)
    (check-equal? (hash-ref r 'verdict) "PENDING")
    (check-equal? (hash-ref v 'status) "pending")
    (check-equal? (hash-ref v 'content-digest) "PENDING")
    (check-equal? (hash-ref e 'review-artifact) (hash-ref v 'review-artifact))
    (check-equal? (hash-ref e 'required-checks) '("lint" "test (0)" "workflows (0)"))
    (check-equal? (hash-ref e 'implementation-sha) (hash-ref e 'merge-sha))
    ;; Complete placeholder scaffold: red-first/focused/fast present, honest.
    (check-equal? (hash-ref (hash-ref v 'red-first) 'command) "PENDING")
    (check-equal? (hash-ref (hash-ref v 'fast) 'result) "PENDING")
    (check-not-false (regexp-match #px"PENDING" (hash-ref (hash-ref v 'focused-tests) 'result)))
    (check-equal? (hash-ref v 'planning-sync) "pending")
    ;; No approval is ever inferred from a draft.
    (check-false (regexp-match? #px"APPROVED" (~s (list e r v)))))

  (test-case "draft refuses missing or malformed required-check snapshots"
    (for ([bad (in-list (list (hash-set draft-request 'required-pr-checks '())
                              (hash-set draft-request 'required-pr-checks "lint")
                              (hash-set draft-request 'required-pr-checks '("lint" "lint"))))])
      (check-exn exn:fail? (lambda () (binding-draft bad)))))

  (test-case "snapshot facts verify the frozen wave and its declared outputs"
    (define dir (plan-tree))
    (bind-snapshot! dir)
    (define facts (snapshot-facts dir campaign-id 1))
    (check-equal? (hash-ref facts 'status) "ok")
    (check-equal? (hash-ref facts 'plan-id) campaign-id)
    (check-equal? (hash-ref facts 'wave) 1)
    (check-equal? (hash-ref facts 'wave-doc) "waves/W1-fixture.md")
    (check-equal? (hash-ref facts 'issue) 9686)
    (check-equal? (hash-ref facts 'milestone) 895)
    (check-equal? (hash-ref (hash-ref facts 'declared) 'evidence)
                  '("q/docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd"))
    (check-equal? (hash-ref (hash-ref facts 'declared) 'review)
                  '("q/docs/reports/gsd-wave-reviews/v9.9.9-w1.rktd"))
    ;; Frozen content is read from the verified snapshot, not live files.
    (write-text! (build-path dir ".planning" "waves" "W1-fixture.md")
                 "# W1: live tamper\n\n- File: `docs/reports/gsd-wave-evidence/evil.rktd`\n")
    (define still (snapshot-facts dir campaign-id 1))
    (check-equal? (hash-ref (hash-ref still 'declared) 'evidence)
                  '("q/docs/reports/gsd-wave-evidence/v9.9.9-w1.rktd"))
    (delete-directory/files dir))

  (test-case "tampered snapshot content never verifies (trusted module)"
    (define dir (plan-tree))
    (bind-snapshot! dir)
    (write-text! (build-path (snapshot-dir dir campaign-id) "waves" "W1-fixture.md")
                 "tampered frozen bytes\n")
    (check-exn exn:fail? (lambda () (snapshot-facts dir campaign-id 1)))
    (delete-directory/files dir))

  (test-case "campaign and wave identity must match the frozen snapshot"
    (define dir (plan-tree))
    (bind-snapshot! dir)
    (check-exn exn:fail? (lambda () (snapshot-facts dir other-campaign-id 1)))
    ;; A frozen multi-wave manifest resolves each index to its own document.
    (define w2 (snapshot-facts dir campaign-id 2))
    (check-equal? (hash-ref w2 'wave-doc) "waves/W2-later.md")
    (check-exn exn:fail? (lambda () (snapshot-facts "no-such-directory" campaign-id 1)))
    (check-exn exn:fail? (lambda () (snapshot-facts dir campaign-id 7)))
    (check-exn exn:fail? (lambda () (snapshot-facts dir "not-a-plan-id" 1)))
    (check-exn exn:fail? (lambda () (snapshot-facts dir campaign-id -1)))
    (delete-directory/files dir))

  (test-case "undeclared waves report no outputs (caller allows only exact hash source)"
    (define dir (plan-tree #:wave-text "# W1: Fixture wave\n\nNo declared outputs.\n"))
    (bind-snapshot! dir)
    (define facts (snapshot-facts dir campaign-id 1))
    (define declared (hash-ref facts 'declared))
    (check-equal? (hash-ref declared 'evidence) '())
    (check-equal? (hash-ref declared 'review) '())
    (check-equal? (hash-ref declared 'validation) '())
    (check-false (hash-ref facts 'issue #f))
    (check-false (hash-ref facts 'milestone #f))
    (delete-directory/files dir))

  (test-case "ambiguous or partial declarations are reported, never guessed"
    (define dir
      (plan-tree #:wave-text (string-append "# W1: Fixture wave\n\n"
                                            "Branch: `campaign/x-w1`; issue "
                                            "[#1](https://github.com/owner/repo/issues/1).\n\n"
                                            "## Files\n\n"
                                            "- File: `docs/reports/gsd-wave-evidence/a-w1.rktd`\n"
                                            "- File: `docs/reports/gsd-wave-evidence/b-w1.rktd`\n")))
    (bind-snapshot! dir)
    (define declared (hash-ref (snapshot-facts dir campaign-id 1) 'declared))
    (check-equal? (length (hash-ref declared 'evidence)) 2)
    (check-equal? (hash-ref declared 'review) '())
    (delete-directory/files dir)))
