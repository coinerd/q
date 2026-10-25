#lang racket/base
;; @speed fast
;; @timeout 300
;; @suite extensions
;; @covers extensions/gsd/wave-docs.rkt
;; @covers extensions/gsd/wave-executor.rkt
;; @covers extensions/gsd/delivery-verifier.rkt

;; tests/test-gsd-declared-wave-path-contract.rkt — BUG-0082 (R82) declared
;; wave-path identity contract.
;;
;; A wave's document identity is the DECLARED arrow path of its PLAN.md
;; index row; the W<idx>-<slug>.md synthesis exists only for arrowless
;; legacy authoring. One validated resolver serves every reader and the
;; sanctioned status writer, so:
;;   - loader content, verifier Verify reparse, ordinary prompts, recovery
;;     copies, campaign hashes and status selection all observe the SAME
;;     declared bytes (a stale synthesized twin never wins);
;;   - a missing declared target is refused BY NAME (exact .planning/...
;;     path) — never silently replaced by the stale twin or a nonempty
;;     plan fallback;
;;   - a shared multi-entry carrier refuses mutation/selection/validation
;;     BEFORE any status/index mutation, with zero file changes (R2
;;     decision); single-entry declared mappings keep the sanctioned
;;     header+index dual-write, preserving the carrier's own header wave
;;     identity;
;;   - malformed/traversal/absolute arrows and symlinked declared docs are
;;     refused, never silently resolved.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "helpers/private-fixture-templates.rkt" call-with-private-git-environment)
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-executor.rkt"
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt" make-verification-registry)
         "../extensions/gsd/delivery-verifier.rkt")

;; ============================================================
;; Fixture helpers
;; ============================================================

;; Fast, deterministic Verify gates: printf is in the accepted R75
;; vocabulary, exits 0, and carries the identity marker inside the command
;; text itself (the executed marker names its source document).
(define STRONG "printf 'strong-carrier-7f3a\\n'")
(define WEAK "printf 'weak-stale-2b1d\\n'")
(define FALLBACK "printf 'stale-fallback-9c4\\n'")

(define CARRIER-REL "waves/W0-carrier-seven-link.md")
(define STALE-REL "waves/W2-carrier-seven-link.md")

(define (carrier-doc #:status [status "Inbox"])
  (string-append "# Wave 0\nStatus: "
                 status
                 "\n\n"
                 "## Files\n- File: q/ui-core/preferences.rkt\n\n"
                 "## Verify\n- `"
                 STRONG
                 "` green.\n\n"
                 "## Done\n- carrier done criterion\n"))

(define (stale-doc)
  (string-append "# Wave 2\nStatus: Inbox\n\n"
                 "## Files\n- File: q/stale-only.rkt\n\n"
                 "## Verify\n- `"
                 WEAK
                 "` green.\n\n"
                 "## Done\n- weak stale done criterion\n"))

(define (write-text! path text)
  (make-parent-directory* path)
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

;; Arbitrary index rows; each row is a full PLAN.md line.
(define (write-plan-rows! base rows)
  (write-text! (build-path base ".planning" "PLAN.md")
               (string-append "# Plan\n\n" (string-join rows "\n") "\n")))

(define (make-base)
  (define base (make-temporary-file "declared-wave-~a" 'directory))
  (make-directory* (build-path base ".planning" "waves"))
  base)

(define (put-carrier! base #:status [status "Inbox"])
  (write-text! (build-path base ".planning" CARRIER-REL) (carrier-doc #:status status)))

(define (put-stale! base)
  (write-text! (build-path base ".planning" STALE-REL) (stale-doc)))

(define (del-rel! base rel)
  (define p (build-path base ".planning" rel))
  (when (file-exists? p)
    (delete-file p)))

;; Deterministic .planning checksum: every path → exact bytes.
(define (tree-bytes base)
  (sort (for/list ([p (in-directory (build-path base ".planning"))]
                   #:when (file-exists? p))
          (cons (path->string (find-relative-path base p)) (file->bytes p)))
        string<?
        #:key car))

(define (wave2-verify-from-loader base)
  (define plan (load-plan-from-index base))
  (and plan
       (for/first ([w (in-list (gsd-plan-waves plan))]
                   #:when (= (gsd-wave-index w) 2))
         (gsd-wave-verify w))))

;; One lightweight two-tier git checkout (base/q repo) so the verifier's
;; git-root resolution works without any network or real delivery.
(define (make-git-base!)
  (define base (make-base))
  (define q-dir (build-path base "q"))
  (make-directory* q-dir)
  (write-text! (build-path q-dir "ui-core" "preferences.rkt")
               "#lang racket/base\n(provide foo)\n(define foo 1)\n")
  (parameterize ([current-directory q-dir])
    (define git (find-executable-path "git"))
    (unless (zero? (system*/exit-code git "init" "-q" "."))
      (error 'make-git-base! "git init failed")))
  base)

;; Run the verifier's verify check on a synthetic campaign: fresh owned
;; verification registry (deterministic), no branch context, no override.
(define (run-verify-check base wave-idx plan)
  (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                 [current-gsd-delivery-verify-timeout-sec 30]
                 [current-gsd-delivery-verify-command #f]
                 [current-gsd-delivery-branch-context #f]
                 [current-owned-supplemental-verification #f])
    (check-verify-command base wave-idx plan)))

;; Hand-built plan carrying a nonempty verify fallback (explicitly scoped
;; hand-built plans have no authored index; here the index EXISTS and
;; declares the doc, so the fallback must never win).
(define fallback-plan
  (gsd-plan
   (list
    (make-gsd-wave 2 "Carrier wave" "" (list "q/ui-core/preferences.rkt") '() FALLBACK (list "done")))
   #f
   '()
   '()))

;; ============================================================
;; 1. Stale collision — declared bytes win, stale never loads
;; ============================================================

(test-case "stale-collision: loader content is the carrier's, the stale twin never loads"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows!
      base
      (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
            "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
     (put-carrier! base)
     (put-stale! base)
     ;; the loader's verify field carries the DECLARED doc's raw Verify
     ;; section — the carrier's marker, never the stale twin's
     (check-true (string-contains? (wave2-verify-from-loader base) "strong-carrier-7f3a"))
     (check-false (string-contains? (wave2-verify-from-loader base) "weak-stale-2b1d"))
     ;; the declared reader returns the same declared bytes for W2
     (define doc (read-declared-wave-doc base 2))
     (check-true (string-contains? (hash-ref doc 'content) "strong-carrier-7f3a"))
     (check-false (string-contains? (hash-ref doc 'content) "weak-stale-2b1d"))
     ;; missing-index-doc-paths sees both rows satisfied through the carrier
     (check-equal? (missing-index-doc-paths
                    base
                    (parse-plan-index (file->string (build-path base ".planning" "PLAN.md"))))
                   '()))
   (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "stale-collision: verifier reparse executes the carrier marker (public verify check)"
  (define base (make-git-base!))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows!
                   base
                   (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
                         "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
                  (put-carrier! base)
                  (put-stale! base)
                  (define result (run-verify-check base 2 #f))
                  (check-equal? (car result) "verify")
                  (check-true (car (cdr result)) "strong carrier gate passes")
                  (define detail (cdr (cdr result)))
                  (check-true (string-contains? detail "strong-carrier-7f3a"))
                  (check-false (string-contains? detail "weak-stale-2b1d")))
                (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 2. Clean declared-only (single-entry mapping)
;; ============================================================

(test-case "clean declared-only: single-entry mapping loads, verifies and selects"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows!
      base
      (list "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"
            "- [Inbox] W5: Legacy arrowless wave"))
     (put-carrier! base)
     (write-text! (build-path base ".planning" "waves" "W5-legacy-arrowless-wave.md")
                  (string-append "# Wave 5\nStatus: Inbox\n\n"
                                 "## Files\n- File: q/legacy.rkt\n\n"
                                 "## Verify\n- `raco make legacy.rkt`\n\n## Done\n- legacy\n"))
     (check-true (string-contains? (wave2-verify-from-loader base) "strong-carrier-7f3a"))
     (check-equal? (wave-doc-lint-warning-lines base) '())
     (check-equal? (check-status-consistency base) '())
     ;; selection sees the declared doc's header, not a synthesized guess
     (define next (next-inbox-wave base))
     (check-not-false next)
     (check-equal? (wave-index-entry-idx next) 2))
   (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 3. Missing declared target — refusal names the exact path
;; ============================================================

(test-case "missing-declared: loader refuses naming the declared path despite the stale twin"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows!
      base
      (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
            "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
     (put-stale! base)
     (check-exn (lambda (e)
                  (and (exn:fail? e)
                       (string-contains? (exn-message e) ".planning/waves/W0-carrier-seven-link.md")
                       ;; never blames the stale synthesized name
                       (not (string-contains? (exn-message e) "waves/W2-carrier-seven-link.md"))))
                (lambda () (load-plan-from-index base))))
   (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "missing-declared: verifier refuses by name and never runs the plan fallback"
  (define base (make-git-base!))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows!
                   base
                   (list "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
                  (put-stale! base)
                  (define result (run-verify-check base 2 fallback-plan))
                  (check-equal? (car result) "verify")
                  (check-false (car (cdr result)) "missing declared doc refuses the delivery")
                  (define detail (cdr (cdr result)))
                  (check-true (string-contains? detail ".planning/waves/W0-carrier-seven-link.md"))
                  (check-false (string-contains? detail "stale-fallback-9c4"))
                  (check-false (string-contains? detail "weak-stale-2b1d")))
                (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 4. Arrowless legacy synthesis preserved
;; ============================================================

(test-case "arrowless legacy: synthesis is the document, transitions still dual-write"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows! base (list "- [Inbox] W3: Arrowless legacy wave"))
     (write-text! (build-path base ".planning" "waves" "W3-arrowless-legacy-wave.md")
                  (string-append "# Wave 3\nStatus: Inbox\n\n"
                                 "## Files\n- File: q/legacy.rkt\n\n"
                                 "## Verify\n- `"
                                 WEAK
                                 "` green.\n\n## Done\n- legacy\n"))
     (check-false (wave2-verify-from-loader base) "W3 never exists for an arrowless-only plan")
     (define plan (load-plan-from-index base))
     (check-true (string-contains? (gsd-wave-verify (car (gsd-plan-waves plan))) "weak-stale-2b1d"))
     ;; sanctioned dual-write on the synthesized doc keeps its own identity
     (check-true (mark-wave-status! base 3 "DONE"))
     (define doc-text
       (file->string (build-path base ".planning" "waves" "W3-arrowless-legacy-wave.md")))
     (check-true (string-contains? doc-text "# Wave 3\nStatus: DONE"))
     (check-true (string-contains? (file->string (build-path base ".planning" "PLAN.md"))
                                   "[DONE] W3:"))
     (check-equal? (check-status-consistency base) '())
     (check-false (next-inbox-wave base)))
   (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 5. Transition → reload → selection/consistency on the DECLARED doc
;;    (single-entry mapping keeps the sanctioned dual-write; the carrier
;;    header keeps its own wave identity)
;; ============================================================

(test-case "transition: mark-wave-status! writes the declared carrier, header identity preserved"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows!
      base
      (list "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"
            "- [Inbox] W5: Legacy arrowless wave"))
     (put-carrier! base)
     (write-text! (build-path base ".planning" "waves" "W5-legacy-arrowless-wave.md")
                  (string-append "# Wave 5\nStatus: Inbox\n\n"
                                 "## Files\n- File: q/legacy.rkt\n\n"
                                 "## Verify\n- `raco make legacy.rkt`\n\n## Done\n- legacy\n"))
     (check-true (mark-wave-status! base 2 "DONE"))
     ;; carrier PATH preserved; carrier HEADER identity preserved (never
     ;; rewritten to the selected W-number)
     (define carrier-text (file->string (build-path base ".planning" CARRIER-REL)))
     (check-true (string-contains? carrier-text "# Wave 0\nStatus: DONE"))
     (check-false (string-contains? carrier-text "# Wave 2"))
     (check-true (string-contains? carrier-text "strong-carrier-7f3a") "content preserved")
     ;; index row for the WRITING wave updated
     (check-true (string-contains? (file->string (build-path base ".planning" "PLAN.md"))
                                   "[DONE] W2:"))
     ;; reload: W2 is complete through the declared doc; selection moves on
     (define reloaded (load-plan-from-index base))
     (define w2
       (for/first ([w (in-list (gsd-plan-waves reloaded))]
                   #:when (= (gsd-wave-index w) 2))
         w))
     (check-equal? (gsd-wave-status w2) 'completed)
     (define next (next-inbox-wave base))
     (check-not-false next)
     (check-equal? (wave-index-entry-idx next) 5 "next selection skips the completed declared wave")
     (check-equal? (check-status-consistency base) '())
     ;; and the DECLARED status is what a re-read resolves
     (check-equal? (hash-ref (read-declared-wave-doc base 2) 'status) "DONE"))
   (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 6. Shared multi-entry carrier — pre-mutation denial, zero changes
;; ============================================================

(test-case "shared-carrier: mutation refuses before any status/index mutation (zero changes)"
  (define base (make-base))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows!
                   base
                   (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
                         "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
                  (put-carrier! base)
                  (put-stale! base)
                  (define before (tree-bytes base))
                  (check-false (mark-wave-status! base 2 "DONE"))
                  (check-false (mark-wave-status! base 0 "DONE"))
                  (check-equal? (tree-bytes base) before "zero file changes"))
                (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "shared-carrier: selection refuses the ambiguous mapping by name"
  (define base (make-base))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows!
                   base
                   (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
                         "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
                  (put-carrier! base)
                  (put-stale! base)
                  (check-exn (lambda (e)
                               (and (exn:fail? e)
                                    (string-contains? (exn-message e) "wave selection refused")
                                    (string-contains? (exn-message e)
                                                      ".planning/waves/W0-carrier-seven-link.md")))
                             (lambda () (next-inbox-wave base))))
                (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "shared-carrier: /go validation refuses the ambiguous mapping as an error finding"
  (define base (make-base))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows!
                   base
                   (list "- [Inbox] W0: carrier → waves/W0-carrier-seven-link.md"
                         "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
                  (put-carrier! base)
                  (put-stale! base)
                  (define result (validate-plan-artifacts base))
                  (check-false (hash-ref result 'ok?))
                  (define errors (plan-validation-error-findings (hash-ref result 'findings)))
                  (check-true (ormap (lambda (f)
                                       (string-contains? (plan-validation-finding-message f)
                                                         ".planning/waves/W0-carrier-seven-link.md"))
                                     errors)))
                (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "duplicate-index: refused by mapping validation, selection and mutation"
  (define base (make-base))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows! base
                                    (list "- [Inbox] W2: First twin → waves/W2-first-twin.md"
                                          "- [Inbox] W2: Second twin → waves/W2-second-twin.md"))
                  (write-text! (build-path base ".planning" "waves" "W2-first-twin.md")
                               "# Wave 2\nStatus: Inbox\n\nBody one.\n")
                  (write-text! (build-path base ".planning" "waves" "W2-second-twin.md")
                               "# Wave 2\nStatus: Inbox\n\nBody two.\n")
                  (check-equal? (length (declared-mapping-refusals
                                         (parse-plan-index
                                          (file->string (build-path base ".planning" "PLAN.md")))))
                                1)
                  (check-exn exn:fail? (lambda () (next-inbox-wave base)))
                  (define before (tree-bytes base))
                  (check-false (mark-wave-status! base 2 "DONE"))
                  (check-equal? (tree-bytes base) before))
                (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 7. Escape / absolute / symlink refusals — named, never fallback
;; ============================================================

(test-case "unsafe arrows: escape, absolute and malformed declarations are refused by name"
  ;; pure validation layer first: substring extraction must not leak
  (check-equal?
   (unsafe-declared-path-refusal "waves/../../evil.md")
   "declared wave document path waves/../../evil.md escapes .planning/waves (.. segment)")
  (check-true (string-contains? (unsafe-declared-path-refusal "/tmp/waves/evil.md") "absolute"))
  (check-true (string-contains? (unsafe-declared-path-refusal "elsewhere/x.md") "not under waves/"))
  (check-false (unsafe-declared-path-refusal "waves/W0-fine.md"))
  (check-false (unsafe-declared-path-refusal "waves/nested/custom-legal.md"))
  (check-false (unsafe-declared-path-refusal #f)))

(test-case "escape arrow: loader refuses naming the declared target, never the synthesis"
  (define base (make-base))
  (dynamic-wind void
                (lambda ()
                  (write-plan-rows! base (list "- [Inbox] W0: Escape → waves/../../evil.md"))
                  (put-stale! base) ; a same-shape twin on disk must not be consulted
                  (check-exn (lambda (e)
                               (and (exn:fail? e)
                                    (string-contains? (exn-message e) "waves/../../evil.md")
                                    (not (string-contains? (exn-message e) "W2-carrier-seven-link"))))
                             (lambda () (load-plan-from-index base)))
                  ;; the mutation path refuses too, with zero file changes
                  (define before (tree-bytes base))
                  (check-false (mark-wave-status! base 0 "DONE"))
                  (check-equal? (tree-bytes base) before))
                (lambda () (delete-directory/files base #:must-exist? #f))))

(test-case "symlinked declared doc: refused by loader, verifier and writer"
  (define base (make-git-base!))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows!
      base
      (list "- [Inbox] W2: Canary seven-link delivery → waves/W0-carrier-seven-link.md"))
     (put-carrier! base)
     (put-stale! base)
     ;; replace the declared doc with a symlink to the stale twin
     (del-rel! base CARRIER-REL)
     (make-file-or-directory-link (build-path base ".planning" STALE-REL)
                                  (build-path base ".planning" CARRIER-REL))
     (check-exn (lambda (e)
                  (and (exn:fail? e)
                       (string-contains? (exn-message e) ".planning/waves/W0-carrier-seven-link.md")
                       (string-contains? (exn-message e) "symlink")))
                (lambda () (load-plan-from-index base)))
     (define result (run-verify-check base 2 fallback-plan))
     (check-false (car (cdr result)))
     (check-true (string-contains? (cdr (cdr result)) ".planning/waves/W0-carrier-seven-link.md"))
     (define before (tree-bytes base))
     (check-false (mark-wave-status! base 2 "DONE"))
     (check-equal? (tree-bytes base) before))
   (lambda () (delete-directory/files base #:must-exist? #f))))

;; ============================================================
;; 8. Nested/custom legal declared paths work end to end
;; ============================================================

(test-case "nested legal declared path: loads, transitions and stays consistent"
  (define base (make-base))
  (dynamic-wind
   void
   (lambda ()
     (write-plan-rows! base (list "- [Inbox] W1: Nested → waves/custom/nested-legal.md"))
     (write-text! (build-path base ".planning" "waves" "custom" "nested-legal.md")
                  (string-append "# Wave 1\nStatus: Inbox\n\n"
                                 "## Files\n- File: q/nested.rkt\n\n"
                                 "## Verify\n- `"
                                 STRONG
                                 "` green.\n\n## Done\n- nested\n"))
     (define plan (load-plan-from-index base))
     (check-true (string-contains? (gsd-wave-verify (car (gsd-plan-waves plan)))
                                   "strong-carrier-7f3a"))
     (check-true (mark-wave-status! base 1 "DONE"))
     (check-true (string-contains?
                  (file->string (build-path base ".planning" "waves" "custom" "nested-legal.md"))
                  "# Wave 1\nStatus: DONE"))
     (check-equal? (check-status-consistency base) '()))
   (lambda () (delete-directory/files base #:must-exist? #f))))
