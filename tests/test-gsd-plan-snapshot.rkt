#lang racket/base
;; @covers extensions/gsd/plan-snapshot.rkt
;; @speed fast  ;; @suite extensions

(require racket/path
         racket/list
         racket/string
         rackunit
         rackunit/text-ui
         racket/file
         "../extensions/gsd/plan-snapshot.rkt"
         ;; W4: the ONE ownership-marker registry (filename and kind per kind).
         (only-in "../extensions/gsd/planning-artifact-ownership.rkt"
                  planning-artifact-marker-filename)
         (only-in "../extensions/gsd/projection-kernel.rkt"
                  project-wave-doc-update
                  project-plan-index-update)
         (only-in "../util/json/checksum.rkt" sha256-string))
(define plan-text
  (string-append "# Plan\n\n"
                 "- [Inbox] W0: Alpha -> waves/W0-alpha.md\n"
                 "- [Inbox] W1: Beta -> waves/W1-beta.md\n"))

(define wave-0 "# Wave 0\n\nalpha body\n")
(define wave-1 "# Wave 1\n\nbeta body\n")
(define campaign-id (make-string 64 #\a))

;; Imported names used by the W4 lifecycle cases below.

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

;; W4: an ownership marker is only authentic when it is a real marker file whose
;; datum records the creating module's schema-version and kind. The snapshot
;; sweep, /plan supersede rotation, the doctor and archive cleanup share ONE
;; predicate (planning-artifact-ownership.rkt), so a test that expects an owned
;; staging directory to be swept must write the AUTHENTIC record — mere marker
;; existence is not proof of ownership.
(define (write-ownership-marker! dir kind)
  (call-with-output-file (build-path dir (planning-artifact-marker-filename kind))
    (lambda (out)
      (write (hasheq 'schema-version 1 'kind kind 'pid 1 'created-at 0) out))
    #:exists 'truncate))

(define (make-plan-tree)
  (define dir (make-temporary-file "plan-snapshot-~a" 'directory))
  (write-text! (build-path dir ".planning" "PLAN.md") plan-text)
  (write-text! (build-path dir ".planning" "waves" "W0-alpha.md") wave-0)
  (write-text! (build-path dir ".planning" "waves" "W1-beta.md") wave-1)
  dir)

(define plan-snapshot-suite
  (test-suite "gsd-plan-snapshot"

    (test-case "extracts and deduplicates real PLAN.md wave references"
      (check-equal? (plan-references->wave-doc-paths (string-append plan-text
                                                                    "duplicate waves/W0-alpha.md\n"))
                    '("waves/W0-alpha.md" "waves/W1-beta.md")))

    (test-case "extracts canonical dotted and WAVE-prefixed wave references"
      (check-equal?
       (plan-references->wave-doc-paths "waves/W0-racket-9.3.md and waves/WAVE-2-long-form.md")
       '("waves/W0-racket-9.3.md" "waves/WAVE-2-long-form.md")))

    (test-case "seed captures canonical dotted and WAVE-prefixed wave documents"
      (define dir (make-temporary-file "plan-snapshot-dotted-~a" 'directory))
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (string-append "waves/W0-racket-9.3.md\n"
                                  "waves/WAVE-1-long-form.md\n"
                                  "waves/W2-Fix_Bug.md\n"))
      (write-text! (build-path dir ".planning" "waves" "W0-racket-9.3.md") "dotted\n")
      (write-text! (build-path dir ".planning" "waves" "WAVE-1-long-form.md") "long\n")
      (write-text! (build-path dir ".planning" "waves" "W2-Fix_Bug.md") "upper\n")
      (define-values (_bound-path _digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-equal?
       (map snapshot-file-path (plan-snapshot-manifest-files manifest))
       '("PLAN.md" "waves/W0-racket-9.3.md" "waves/WAVE-1-long-form.md" "waves/W2-Fix_Bug.md"))
      (delete-directory/files dir))

    (test-case "read-only snapshot loading neither sweeps nor invents drift"
      (define dir (make-plan-tree))
      (define campaign-dir* (build-path dir ".planning" "campaigns" campaign-id))
      (define stale (build-path campaign-dir* "plan-snapshot.tmp-stale"))
      (make-directory* stale)
      (check-false (load-snapshot-manifest dir campaign-id #:sweep-stale? #f))
      (check-true (directory-exists? stale))
      (check-equal? (classify-snapshot-drift dir campaign-id #:sweep-stale? #f) '())
      ;; W4: a staging-shaped directory without our ownership marker is
      ;; unknown operator state — the legacy explicit load preserves it too.
      (check-false (load-snapshot-manifest dir campaign-id))
      (check-true (directory-exists? stale)
                  "cleanup may only remove PROVEN owned staging directories")
      ;; The interrupted operation of OURS carries the authentic marker and is swept.
      (define owned (build-path campaign-dir* "plan-snapshot.tmp-owned"))
      (make-directory* owned)
      (write-ownership-marker! owned 'plan-snapshot-staging)
      (check-false (load-snapshot-manifest dir campaign-id))
      (check-false (directory-exists? owned) "owned interrupted staging is swept")
      (check-true (directory-exists? stale))
      (delete-directory/files dir))

    (test-case "seed binds and verifies PLAN plus every referenced wave"
      (define dir (make-plan-tree))
      (define-values (bound-path digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? bound-path (path->string (snapshot-dir dir campaign-id)))
      (check-regexp-match #px"^[0-9a-f]+$" digest)
      (check-equal? (string-length digest) 64)
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-equal? (map snapshot-file-path (plan-snapshot-manifest-files manifest))
                    '("PLAN.md" "waves/W0-alpha.md" "waves/W1-beta.md"))
      (write-text! (build-path (snapshot-dir dir campaign-id) "waves" "W0-alpha.md")
                   "corrupt snapshot bytes")
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "snapshot capture is create-once across resume migration"
      (define dir (make-plan-tree))
      (define-values (_path first-digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (define captured-plan (file->string (build-path (snapshot-dir dir campaign-id) "PLAN.md")))
      ;; Unchanged authored content re-binds to the same snapshot verbatim.
      (define-values (_same-path second-digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? second-digest first-digest)
      (check-equal? (file->string (build-path (snapshot-dir dir campaign-id) "PLAN.md"))
                    captured-plan)
      (delete-directory/files dir))

    ;; F13 (this campaign's W5): seed-and-bind reused an existing snapshot verbatim
    ;; even when the authored plan BODY had been amended, silently binding a
    ;; campaign to a frozen contract that no longer matches the plan being
    ;; executed. The freeze must refuse instead.
    (test-case "F13: authored plan-body amendment refuses frozen-contract-stale"
      (define dir (make-plan-tree))
      (define-values (_p first-digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (define amended
        (string-append plan-text "\n## Failure Modes\n\n" "| F14 | body amendment after freeze |\n"))
      (write-text! (build-path dir ".planning" "PLAN.md") amended)
      (check-exn exn:fail:gsd-frozen-contract-stale?
                 (lambda () (seed-and-bind-plan-snapshot! dir campaign-id)))
      (check-equal? (snapshot-manifest-digest (load-snapshot-manifest dir campaign-id)) first-digest)
      (check-equal? (file->string (build-path (snapshot-dir dir campaign-id) "PLAN.md")) plan-text)
      (delete-directory/files dir))

    (test-case "F13: drift classification names the frozen contract as stale"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (string-append plan-text "\nAmended contract clause\n"))
      (check-equal? (snapshot-drift? dir campaign-id) '("PLAN.md"))
      (check-not-false (member (list "PLAN.md" 'frozen-contract-stale)
                               (classify-snapshot-drift dir campaign-id)))
      (delete-directory/files dir))

    (test-case "F13: override restore restores a stale plan body (sanctioned remedy)"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (string-append plan-text "\nAmended contract clause\n"))
      (check-not-false (member (list "PLAN.md" 'frozen-contract-stale)
                               (classify-snapshot-drift dir campaign-id)))
      (define restored (restore-plan-from-snapshot! dir campaign-id #:override-existing-drift? #t))
      (check-not-false (member "PLAN.md" restored)
                       "the documented remedy restores the stale contract")
      (check-equal? (file->string (build-path dir ".planning" "PLAN.md")) plan-text)
      (check-equal? (snapshot-drift? dir campaign-id) '())
      (delete-directory/files dir))

    (test-case "F13: amended wave docs still classify as content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (write-text! (build-path dir ".planning" "waves" "W1-beta.md")
                   "# Wave 1\n\nbeta body changed\n")
      (check-not-false (member (list "waves/W1-beta.md" 'content-drift)
                               (classify-snapshot-drift dir campaign-id)))
      (delete-directory/files dir))

    (test-case "manifest traversal entry is rejected before external access"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define outside (build-path dir "outside.txt"))
      (write-text! outside "owned")
      (define manifest-path (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd"))
      (define datum (call-with-input-file manifest-path read))
      (define evil-file
        (hasheq 'path "../../../../outside.txt" 'size 5 'sha256 (sha256-string "owned")))
      (call-with-output-file manifest-path
                             (lambda (out) (write (hash-set datum 'files (list evil-file)) out))
                             #:exists 'truncate)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "symlinked snapshot entry is rejected even when bytes match"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define outside (build-path dir "outside-wave.md"))
      (write-text! outside wave-0)
      (define captured (build-path (snapshot-dir dir campaign-id) "waves" "W0-alpha.md"))
      (delete-file captured)
      (make-file-or-directory-link outside captured)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "restore rejects dangling symlink destinations without following them"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-plan (build-path dir ".planning" "PLAN.md"))
      (define outside (build-path dir "outside-plan.md"))
      (delete-file live-plan)
      (make-file-or-directory-link outside live-plan)
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-false (file-exists? outside))
      (delete-directory/files dir))

    (test-case "restore rejects a symlinked live parent directory"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-waves (build-path dir ".planning" "waves"))
      (define outside (build-path dir "outside-waves"))
      (delete-directory/files live-waves)
      (make-directory outside)
      (make-file-or-directory-link outside live-waves)
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-false (file-exists? (build-path outside "W0-alpha.md")))
      (delete-directory/files dir))

    (test-case "missing referenced wave fails before writing a snapshot"
      (define dir (make-plan-tree))
      (delete-file (build-path dir ".planning" "waves" "W1-beta.md"))
      (check-exn exn:fail:gsd-missing-wave-doc?
                 (lambda () (seed-and-bind-plan-snapshot! dir campaign-id)))
      (check-false (directory-exists? (snapshot-dir dir campaign-id)))
      (delete-directory/files dir))

    (test-case "status projection changes do not count as content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (regexp-replace* (regexp (regexp-quote "[Inbox]")) plan-text "[DONE]"))
      (write-text! (build-path dir ".planning" "waves" "W0-alpha.md")
                   (string-append "# Wave 0\nStatus: FAILED\n\n"
                                  wave-0
                                  "\n## Last Failure\n\nverifier rejected\n"))
      (check-equal? (snapshot-drift? dir campaign-id) '())
      (write-text! (build-path dir ".planning" "waves" "W0-alpha.md")
                   "# Wave 0\nStatus: DONE\n\nchanged body\n")
      (check-equal? (snapshot-drift? dir campaign-id) '("waves/W0-alpha.md"))
      (delete-directory/files dir))

    (test-case "restore replaces missing files but rejects existing content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define plan-path (build-path dir ".planning" "PLAN.md"))
      (define wave-path (build-path dir ".planning" "waves" "W0-alpha.md"))
      (write-text! plan-path (regexp-replace (regexp (regexp-quote "[Inbox]")) plan-text "[DONE]"))
      (delete-file wave-path)
      (check-equal? (restore-plan-from-snapshot! dir campaign-id) '("waves/W0-alpha.md"))
      (check-equal? (file->string wave-path) wave-0)
      (check-true (string-contains? (file->string plan-path) "[DONE]"))
      (write-text! wave-path "# Wave 0\nStatus: DONE\n\nmalicious edit\n")
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-regexp-match #rx"malicious edit" (file->string wave-path))
      (delete-directory/files dir))

    (test-case "classify-snapshot-drift distinguishes missing from content drift"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (delete-file (build-path dir ".planning" "waves" "W0-alpha.md"))
      (write-text! (build-path dir ".planning" "waves" "W1-beta.md")
                   "# Wave 1\n\nexternally edited body\n")
      (define classified (classify-snapshot-drift dir campaign-id))
      (check-not-false (member (list "waves/W0-alpha.md" 'missing) classified)
                       "missing file classified")
      (check-not-false (member (list "waves/W1-beta.md" 'content-drift) classified)
                       "edited file classified as content drift")
      (delete-directory/files dir))

    (test-case "override restore rewrites existing drift with snapshot content"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define wave-path (build-path dir ".planning" "waves" "W0-alpha.md"))
      ;; The BUG-0068 incident shape: a self-authored wave-doc annotation
      ;; blocks resume; the sanctioned remedy must be executable.
      (write-text! wave-path (string-append wave-0 "\nAnnotated: attempt bookkeeping\n"))
      (check-equal? (snapshot-drift? dir campaign-id) '("waves/W0-alpha.md"))
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (define restored (restore-plan-from-snapshot! dir campaign-id #:override-existing-drift? #t))
      (check-equal? restored '("waves/W0-alpha.md"))
      (check-equal? (file->string wave-path) wave-0)
      (check-equal? (snapshot-drift? dir campaign-id) '())
      (delete-directory/files dir))

    (test-case "override restore still refuses symlink boundaries"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-plan (build-path dir ".planning" "PLAN.md"))
      (define outside (build-path dir "outside-plan.md"))
      (delete-file live-plan)
      (make-file-or-directory-link outside live-plan)
      (define refusal-message
        (with-handlers ([exn:fail? exn-message])
          (restore-plan-from-snapshot! dir campaign-id #:override-existing-drift? #t)
          #f))
      (check-true (string? refusal-message))
      (check-true (string-contains? refusal-message "symlink")
                  "the refusal names the non-overridable class")
      (check-true (string-contains? refusal-message "PLAN.md")
                  "the refusal names the path")
      (check-false (file-exists? outside))
      (delete-directory/files dir))

    ;; ── W4: every generated projection block is status-neutral ──────────
    ;;
    ;; CHARACTERIZATION (hypothesis, now pinned): the wave-doc projection
    ;; writes a MULTILINE header block (`# Wave N` + `Status: ...` + blank
    ;; line). A doc whose authored header carries extra lines (a second
    ;; `Status:` marker, an `Attempt:` line, no blank separator) does not
    ;; match the header-rewrite regexp, so the projection PREPENDS its block
    ;; instead of replacing it. Normalization must therefore treat a leading
    ;; generated header block as projection state — repeated blocks included —
    ;; or a pure status projection is misreported as authored content drift.

    (test-case "W4: leading generated header blocks are projection state (multiline header)"
      (define authored "# Wave 0\n\nGoal: alpha\n\n## Verify\n\nraco test .\n")
      (check-equal? (normalize-wave-doc-content (string-append "# Wave 0\nStatus: Inbox\n\n" authored))
                    (normalize-wave-doc-content authored)
                    "one generated header block is projection state")
      (check-equal? (normalize-wave-doc-content (string-append "# Wave 0\nStatus: Inbox\n\n"
                                                              "# Wave 0\nStatus: DONE\n\n"
                                                              authored))
                    (normalize-wave-doc-content authored)
                    "repeated generated header blocks are projection state too"))

    (test-case "W4: headingless authored docs stay status-neutral after projection"
      (define authored "Goal: alpha\n\n## Verify\n\nraco test .\n")
      (check-equal? (normalize-wave-doc-content (project-wave-doc-update authored 0 "Inbox"))
                    (normalize-wave-doc-content authored)))

    (test-case "W4: authored extra header lines do not turn a status projection into drift"
      ;; Legacy template residue and authored `Attempt:` metadata: the
      ;; projection prepends its managed block instead of rewriting these.
      (for ([authored (in-list (list "# Wave 0\nStatus: PENDING\nStatus: STALE\n\nbody\n"
                                     "# Wave 0\nStatus: Inbox\nAttempt: 2\n\nbody\n"
                                     "# Wave 0\nStatus: Inbox\nbody\n"))])
        (define projected (project-wave-doc-update authored 0 "DONE"))
        (check-equal? (normalize-wave-doc-content projected)
                      (normalize-wave-doc-content authored)
                      (format "status-only projection of ~s must not drift" authored)))
      ;; CHARACTERIZED mechanism: extra authored header lines defeat the
      ;; header REWRITE, so the managed block is PREPENDED (two headings).
      (check-equal? (regexp-match* #px"(?m:^# Wave 0$)"
                                   (project-wave-doc-update
                                    "# Wave 0\nStatus: PENDING\nStatus: STALE\n\nbody\n"
                                    0 "DONE"))
                    '("# Wave 0" "# Wave 0")
                    "the projection prepends its multiline managed block"))

    (test-case "W4: an authored `## Last Failure` only hides its own section"
      ;; `record-wave-failure!` rewrites the failure section in place. The
      ;; normalization may ignore that section, but never the authored
      ;; content that follows it.
      (define authored (string-append "# Wave 0\nStatus: DONE\n\nbody\n\n"
                                      "## Last Failure\nold reason\n\n"
                                      "## Verify\n\nraco test .\n"))
      (define replayed (string-append "# Wave 0\nStatus: DONE\n\nbody\n\n"
                                      "## Last Failure\nnew reason\n\n"
                                      "## Verify\n\nraco test .\n"))
      (check-equal? (normalize-wave-doc-content replayed)
                    (normalize-wave-doc-content authored)
                    "the failure section body is projection state")
      (define edited-after-failure
        (string-append "# Wave 0\nStatus: DONE\n\nbody\n\n"
                       "## Last Failure\nnew reason\n\n"
                       "## Verify\n\nraco test EDITED.rkt\n"))
      (check-not-equal? (normalize-wave-doc-content edited-after-failure)
                        (normalize-wave-doc-content authored)
                        "authored content after the failure section is still compared"))

    (test-case "W4: PLAN index status markers are the only normalized PLAN bytes"
      (define base (string-append "# Plan\n\n"
                                  "- [Inbox] W0: Alpha -> waves/W0-a.md\n"
                                  "* [Inbox] W1: Beta -> waves/W1-b.md\n"))
      (define progressed (string-append "# Plan\n\n"
                                        "- [DONE] W0: Alpha -> waves/W0-a.md\n"
                                        "* [FAILED] W1: Beta -> waves/W1-b.md\n"))
      (check-equal? (plan-body-normalized-hash progressed) (plan-body-normalized-hash base)
                    "index status markers are projection state")
      (check-not-equal? (plan-body-normalized-hash
                         (string-append base "\n## Notes\n\nauthored addition\n"))
                        (plan-body-normalized-hash base)
                        "authored PLAN bytes still count as identity"))

    (test-case "W4: replaying status projections changes no bound identity"
      (define dir (make-plan-tree))
      (define-values (_path digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (define plan-path (build-path dir ".planning" "PLAN.md"))
      (define doc-path (build-path dir ".planning" "waves" "W0-alpha.md"))
      ;; Exactly what the projection shell writes on completion/reconcile.
      (write-text! plan-path (project-plan-index-update (file->string plan-path) 0 "DONE"))
      (write-text! doc-path (project-wave-doc-update (file->string doc-path) 0 "DONE"))
      (check-equal? (snapshot-drift? dir campaign-id) '()
                    "status-only projection replay is informational non-drift")
      (check-equal? (snapshot-manifest-digest (load-snapshot-manifest dir campaign-id)) digest)
      (delete-directory/files dir))

    (test-case "W4: drift taxonomy is explicit and only overridable classes are overridable"
      (check-equal? (snapshot-drift-classes)
                    '(symlink missing content-drift frozen-contract-stale))
      (check-false (snapshot-drift-class-overridable? 'symlink)
                   "the trust-boundary class is never overridable")
      (for ([kind (in-list '(missing content-drift frozen-contract-stale))])
        (check-true (snapshot-drift-class-overridable? kind))
        (check-true (string? (snapshot-drift-class-description kind))
                    "every class names itself for the operator"))
      ;; The classes stay DISTINCT: no smaller ambiguous `structural` bucket.
      (check-equal? (length (remove-duplicates (map snapshot-drift-class-description
                                                    (snapshot-drift-classes)) string=?))
                    4))

    (test-case "W4: a malformed stored manifest is storage corruption, not a drift class"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define manifest-path (build-path (snapshot-dir dir campaign-id)
                                        "snapshot-manifest.rktd"))
      (write-text! manifest-path "not-a-manifest\n")
      (check-exn exn:fail? (lambda () (classify-snapshot-drift dir campaign-id)))
      (delete-directory/files dir))

    (test-case "W4: snapshot staging cleanup removes only proven owned artifacts"
      (define dir (make-plan-tree))
      (define campaigns-dir (build-path dir ".planning" "campaigns" campaign-id))
      (make-directory* campaigns-dir)
      ;; An interrupted operation of OURS (carries the AUTHENTIC ownership marker).
      (define owned (build-path campaigns-dir "plan-snapshot.tmp-owned"))
      (make-directory* owned)
      (write-ownership-marker! owned 'plan-snapshot-staging)
      ;; Unknown artifacts that merely share the name shape, plus user backups.
      (define unknown (build-path campaigns-dir "plan-snapshot.tmp-unknown"))
      (make-directory* unknown)
      (write-text! (build-path unknown "notes.md") "unknown payload\n")
      (define backup (build-path campaigns-dir "waves-pre-plan-backup"))
      (make-directory* backup)
      (write-text! (build-path backup "W0-old.md.bak") "precious\n")
      (check-false (load-snapshot-manifest dir campaign-id))
      (check-false (directory-exists? owned) "owned staging is swept")
      (check-true (directory-exists? unknown) "unknown staging-shaped dir is preserved")
      (check-true (file-exists? (build-path unknown "notes.md")))
      (check-true (file-exists? (build-path backup "W0-old.md.bak"))
                  "unknown waves-pre-plan-backup and .bak files are preserved")
      (delete-directory/files dir))

    ;; W4 BLOCKER (RED): marker EXISTENCE used to authorize the sweep, so an
    ;; impostor marker (wrong kind or wrong schema) deleted operator state.
    (test-case "W4: an impostor ownership marker never authorizes a staging sweep"
      (define dir (make-plan-tree))
      (define campaigns-dir (build-path dir ".planning" "campaigns" campaign-id))
      (make-directory* campaigns-dir)
      (define (impostor name marker-text)
        (define d (build-path campaigns-dir name))
        (make-directory* d)
        (write-text! (build-path d plan-snapshot-staging-marker-filename) marker-text)
        (write-text! (build-path d "payload.md") "operator payload\n")
        d)
      (define wrong-kind
        (impostor "plan-snapshot.tmp-wrong-kind"
                  "#hash((schema-version . 1) (kind . waves-pre-plan-backup))\n"))
      (define wrong-schema
        (impostor "plan-snapshot.tmp-wrong-schema"
                  "#hash((schema-version . 2) (kind . plan-snapshot-staging))\n"))
      (define empty-marker (impostor "plan-snapshot.tmp-empty" "\n"))
      (check-false (load-snapshot-manifest dir campaign-id))
      (for ([d (in-list (list wrong-kind wrong-schema empty-marker))])
        (check-true (directory-exists? d)
                    (format "~a must survive an impostor marker" (path->string d)))
        (check-true (file-exists? (build-path d "payload.md"))))
      (delete-directory/files dir))))

(void (run-tests plan-snapshot-suite))
