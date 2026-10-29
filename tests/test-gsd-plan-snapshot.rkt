#lang racket/base
;; @covers extensions/gsd/plan-snapshot.rkt
;; @speed fast  ;; @suite extensions

(require racket/path
         racket/runtime-path
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

;; ── v1 (d5bf128b) snapshot digest scheme, transcribed VERBATIM ─────────
;; Independent of the module under test: a later refactor of
;; normalize-wave-doc-content must not silently redefine what "legacy" means.
(define legacy-wave-doc-status-header-rx #rx"^(# Wave [0-9]+\n)Status: [^\n]+\n")
(define legacy-plan-index-status-rx #px"(?m:^([-*] +)\\[[A-Za-z-]+\\]( +W[0-9]+:.*)$)")

(define (legacy-canonical-lines text)
  (string-join (string-split text "\n") "\n"))

(define (legacy-normalize-wave-doc-content text)
  (define without-status (regexp-replace legacy-wave-doc-status-header-rx text "\\1"))
  (define lines (string-split without-status "\n" #:trim? #f))
  (define first-heading (and (pair? lines) (regexp-match #rx"^# Wave ([0-9]+)(?::.*)?$" (car lines))))
  (define authored-heading
    (and (>= (length lines) 3) (regexp-match #rx"^#+ Wave ([0-9]+)(?::.*)?$" (caddr lines))))
  (define without-generated-prefix
    (if (and first-heading
             authored-heading
             (string=? (cadr lines) "")
             (equal? (cadr first-heading) (cadr authored-heading)))
        (cddr lines)
        lines))
  (define normalized (legacy-canonical-lines (string-join without-generated-prefix "\n")))
  (define failure-marker (regexp-match-positions #rx"\n+## Last Failure\n" normalized))
  (if failure-marker
      (substring normalized 0 (caar failure-marker))
      normalized))

(define (legacy-normalized-file-hash rel-path raw-text)
  (sha256-string (cond
                   [(string-prefix? rel-path "waves/") (legacy-normalize-wave-doc-content raw-text)]
                   [(equal? rel-path "PLAN.md")
                    (legacy-canonical-lines
                     (regexp-replace* legacy-plan-index-status-rx raw-text "\\1[STATUS]\\2"))]
                   [else raw-text])))

(define (current-normalized-file-hash rel-path raw-text)
  (cond
    [(string-prefix? rel-path "waves/") (sha256-string (normalize-wave-doc-content raw-text))]
    [(equal? rel-path "PLAN.md") (plan-body-normalized-hash raw-text)]
    [else (sha256-string raw-text)]))

;; ── Authentic v1 baseline fixture ───────────────────────────────────────
;;
;; Campaign 79a69b42 (this repository) captured
;; `waves/W0-tracker-reconciliation-wiring.md` with the v1 writer at
;; d5bf128b. `tests/fixtures/plan-snapshot-legacy-v1/` holds those captured
;; bytes verbatim (SIZE below), and LEGACY-V1-W0-DIGEST is the digest that
;; campaign's manifest recorded for them. Hashing the SAME bytes with the
;; 4ac41abc normalizer yields a DIFFERENT digest
;; (LEGACY-V1-W0-CURRENT-DIGEST): that divergence, with no persisted
;; schema/version change, made an authentic capture read as
;; "partial/corrupt". Both facts are pinned below, so this fixture fails
;; loudly if either transcription or normalizer drifts.
(define legacy-v1-w0-rel "waves/W0-tracker-reconciliation-wiring.md")
(define legacy-v1-w0-digest "3b7a40404b84efa40343feedc6081692454bcc504eead643124d8907ba0b43d5")
;; What commit 4ac41abc's normalizer produced for the SAME captured bytes.
(define legacy-v1-w0-current-digest
  "7dd8b79de08b65c7d382ea12e98e0a88e6c8176b697c7e77852df4b55dc8955f")
(define legacy-v1-w0-size 4584)
(define-runtime-path legacy-v1-w0-fixture
                     "fixtures/plan-snapshot-legacy-v1/waves/W0-tracker-reconciliation-wiring.md")
(define legacy-v1-plan-text
  (string-append "# Plan\n\n" "- [Inbox] W0: Tracker reconciliation -> " legacy-v1-w0-rel "\n"))

;; A digest as the named enumerable scheme would have rendered it. Used to
;; install captures of DIFFERENT historical writers into the same
;; schema-version-1 manifest shape.
(define (scheme-normalized-file-hash rel-path raw-text digest-scheme)
  (case digest-scheme
    [(1) (legacy-normalized-file-hash rel-path raw-text)]
    [(2) (current-normalized-file-hash rel-path raw-text)]
    [else (error 'scheme-normalized-file-hash "unknown digest scheme ~s" digest-scheme)]))

(define (legacy-v1-file-entry rel frozen-path #:digest-scheme [digest-scheme 1])
  `#hasheq((path . ,rel)
           (size . ,(file-size frozen-path))
           (sha256 . ,(scheme-normalized-file-hash rel (file->string frozen-path) digest-scheme))))

;; Install a capture in the schema-version-1 manifest shape — NO `hash-scheme`
;; datum, exactly what every writer up to and including 4ac41abc persisted.
;; Live and frozen bytes are identical. `#:digest-scheme` selects WHICH known
;; writer produced the recorded digests: 1 = the authentic capture of campaign
;; 79a69b42 (d5bf128b), 2 = a 4ac41abc capture with the same unrecorded shape.
;; `#:manifest-scheme` deliberately mislabels the digests, for the strict-verify
;; cases.
(define (install-v1-capture! dir
                             #:manifest-scheme [manifest-scheme #f]
                             #:digest-scheme [digest-scheme 1]
                             #:captured [captured-text (file->string legacy-v1-w0-fixture)])
  (define frozen-plan (build-path (snapshot-dir dir campaign-id) "PLAN.md"))
  (define frozen-wave (build-path (snapshot-dir dir campaign-id) legacy-v1-w0-rel))
  (write-text! (build-path dir ".planning" "PLAN.md") legacy-v1-plan-text)
  (write-text! (build-path dir ".planning" legacy-v1-w0-rel) captured-text)
  (write-text! frozen-plan legacy-v1-plan-text)
  (write-text! frozen-wave captured-text)
  (define base
    `#hasheq((schema-version . 1)
             (campaign . ,campaign-id)
             (created-at . 0)
             (plan-id . ,campaign-id)
             (files
              .
              (#hasheq((path . "PLAN.md")
                       (size . ,(file-size frozen-plan))
                       (sha256
                        . ,(scheme-normalized-file-hash "PLAN.md" legacy-v1-plan-text digest-scheme)))
               ,(legacy-v1-file-entry legacy-v1-w0-rel frozen-wave #:digest-scheme digest-scheme)))))
  (call-with-output-file (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd")
                         (lambda (out)
                           (write (if manifest-scheme
                                      (hash-set base 'hash-scheme manifest-scheme)
                                      base)
                                  out))
                         #:exists 'truncate)
  dir)

(define (make-legacy-v1-tree)
  (install-v1-capture! (make-temporary-file "plan-snapshot-legacy-~a" 'directory)))
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
      (check-true (string-contains? refusal-message "PLAN.md") "the refusal names the path")
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
      (check-equal? (normalize-wave-doc-content (string-append "# Wave 0\nStatus: Inbox\n\n"
                                                               authored))
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
      (check-equal?
       (regexp-match*
        #px"(?m:^# Wave 0$)"
        (project-wave-doc-update "# Wave 0\nStatus: PENDING\nStatus: STALE\n\nbody\n" 0 "DONE"))
       '("# Wave 0" "# Wave 0")
       "the projection prepends its multiline managed block"))

    (test-case "W4: an authored `## Last Failure` only hides its own section"
      ;; `record-wave-failure!` rewrites the failure section in place. The
      ;; normalization may ignore that section, but never the authored
      ;; content that follows it.
      (define authored
        (string-append "# Wave 0\nStatus: DONE\n\nbody\n\n"
                       "## Last Failure\nold reason\n\n"
                       "## Verify\n\nraco test .\n"))
      (define replayed
        (string-append "# Wave 0\nStatus: DONE\n\nbody\n\n"
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
      (define base
        (string-append "# Plan\n\n"
                       "- [Inbox] W0: Alpha -> waves/W0-a.md\n"
                       "* [Inbox] W1: Beta -> waves/W1-b.md\n"))
      (define progressed
        (string-append "# Plan\n\n"
                       "- [DONE] W0: Alpha -> waves/W0-a.md\n"
                       "* [FAILED] W1: Beta -> waves/W1-b.md\n"))
      (check-equal? (plan-body-normalized-hash progressed)
                    (plan-body-normalized-hash base)
                    "index status markers are projection state")
      (check-not-equal?
       (plan-body-normalized-hash (string-append base "\n## Notes\n\nauthored addition\n"))
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
      (check-equal? (snapshot-drift? dir campaign-id)
                    '()
                    "status-only projection replay is informational non-drift")
      (check-equal? (snapshot-manifest-digest (load-snapshot-manifest dir campaign-id)) digest)
      (delete-directory/files dir))

    (test-case "W4: drift taxonomy is explicit and only overridable classes are overridable"
      (check-equal? (snapshot-drift-classes) '(symlink missing content-drift frozen-contract-stale))
      (check-false (snapshot-drift-class-overridable? 'symlink)
                   "the trust-boundary class is never overridable")
      (for ([kind (in-list '(missing content-drift frozen-contract-stale))])
        (check-true (snapshot-drift-class-overridable? kind))
        (check-true (string? (snapshot-drift-class-description kind))
                    "every class names itself for the operator"))
      ;; The classes stay DISTINCT: no smaller ambiguous `structural` bucket.
      (check-equal? (length (remove-duplicates (map snapshot-drift-class-description
                                                    (snapshot-drift-classes))
                                               string=?))
                    4))

    (test-case "W4: a malformed stored manifest is storage corruption, not a drift class"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define manifest-path (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd"))
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
      (delete-directory/files dir))

    ;; ── Snapshot digest-scheme compatibility (legacy v1 ↔ current) ──────
    ;;
    ;; BUG (RED before this fix): campaign 79a69b42 captured its waves with the
    ;; v1 normalizer at d5bf128b. When 4ac41abc changed the wave normalization
    ;; without changing the manifest schema, `load-snapshot-manifest` re-hashed
    ;; the AUTHENTIC captured bytes with the NEW scheme, compared them to the
    ;; recorded v1 digest, and reported "partial/corrupt snapshot entry" — the
    ;; live campaign became unresumable. Authentic bytes were never wrong; the
    ;; READER only knew one rendering.

    (test-case "legacy v1 baseline fixture reproduces the authentic capture digests"
      (define captured (file->string legacy-v1-w0-fixture))
      (check-equal? (file-size legacy-v1-w0-fixture)
                    legacy-v1-w0-size
                    "the fixture is a byte-exact copy of the authentic capture")
      (check-equal? (legacy-normalized-file-hash legacy-v1-w0-rel captured)
                    legacy-v1-w0-digest
                    "the v1 transcription reproduces the digest the campaign recorded")
      (check-not-equal? (current-normalized-file-hash legacy-v1-w0-rel captured)
                        legacy-v1-w0-digest
                        "the 4ac41abc normalization disagrees on the same bytes")
      (check-equal? (current-normalized-file-hash legacy-v1-w0-rel captured)
                    legacy-v1-w0-current-digest
                    "4ac41abc's rendering of the same bytes is pinned")
      (check-equal? (plan-body-normalized-hash legacy-v1-plan-text)
                    (legacy-normalized-file-hash "PLAN.md" legacy-v1-plan-text)
                    "PLAN.md normalization is scheme-invariant"))

    (test-case "a scheme-2 capture that never recorded a scheme is accepted and digest-stable"
      ;; 4ac41abc's writer produced scheme-2 digests into the SAME
      ;; schema-version-1 manifest shape with NO `hash-scheme` datum. The loader
      ;; must authenticate that shape against the ENUMERATED schemes (current
      ;; matches by exact digest), report no drift, and leave the stored datum —
      ;; whose digest the durable campaign record binds — byte-identical across
      ;; reuse. Without the enumerated-scheme check this capture would look
      ;; "partial/corrupt", which is the compat regression itself.
      (define dir (make-temporary-file "plan-snapshot-unrecorded-scheme2-~a" 'directory))
      (install-v1-capture! dir #:digest-scheme 2)
      (check-false (hash-has-key? (call-with-input-file (build-path (snapshot-dir dir campaign-id)
                                                                    "snapshot-manifest.rktd")
                                                        read)
                                  'hash-scheme)
                   "the fixture records no digest scheme")
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-true (plan-snapshot-manifest? manifest)
                  "a scheme-2 digest with no declared scheme is accepted")
      (check-false (plan-snapshot-manifest-hash-scheme manifest)
                   "loading never invents a recorded scheme")
      (check-equal? (for/first ([f (in-list (plan-snapshot-manifest-files manifest))]
                                #:when (equal? (snapshot-file-path f) legacy-v1-w0-rel))
                      (snapshot-file-sha256 f))
                    legacy-v1-w0-current-digest
                    "the recorded digest is 4ac41abc's scheme-2 rendering")
      (check-equal? (classify-snapshot-drift dir campaign-id) '())
      (check-equal? (snapshot-drift? dir campaign-id) '())
      ;; Digest stability: the manifest digest is the digest of the stored datum
      ;; and reuse binds the same value without rewriting the frozen bytes.
      (define stored
        (call-with-input-file (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd")
                              read))
      (check-equal? (snapshot-manifest-digest manifest) (sha256-string (format "~s" stored)))
      (define manifest-file (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd"))
      (define before (file->bytes manifest-file))
      (define-values (_path digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? digest (snapshot-manifest-digest manifest) "reuse binds the same manifest digest")
      (check-equal? (file->bytes manifest-file) before "reuse never rewrites the frozen manifest")
      (check-equal? (call-with-input-file manifest-file read) stored)
      (delete-directory/files dir))

    (test-case "legacy v1 capture loads and reports no drift (baseline fail->pass)"
      (define dir (make-legacy-v1-tree))
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-true (plan-snapshot-manifest? manifest)
                  "an authentic v1 capture is readable, not partial/corrupt")
      (check-false (plan-snapshot-manifest-hash-scheme manifest)
                   "a legacy manifest records no scheme")
      (check-equal? (classify-snapshot-drift dir campaign-id) '())
      (check-equal? (snapshot-drift? dir campaign-id) '())
      ;; Digest stability: the durable campaign record binds this digest, so a
      ;; legacy manifest must re-serialize to the exact datum it was stored as.
      (define stored
        (call-with-input-file (build-path (snapshot-dir dir campaign-id) "snapshot-manifest.rktd")
                              read))
      (check-false (hash-has-key? stored 'hash-scheme))
      (check-equal? (snapshot-manifest-digest manifest) (sha256-string (format "~s" stored)))
      (delete-directory/files dir))

    (test-case "legacy v1 capture: status projections stay non-drift"
      (define dir (make-legacy-v1-tree))
      (define live-path (build-path dir ".planning" legacy-v1-w0-rel))
      (define captured (file->string legacy-v1-w0-fixture))
      ;; MULTILINE on purpose: `Status:` is the SECOND line of every wave doc,
      ;; so an anchored `^Status:` without `(?m:` matches nothing and would make
      ;; this case assert on unchanged bytes. The replacement is checked to have
      ;; actually happened before drift is classified.
      (define projected (regexp-replace #rx"(?m:^Status: [^\n]*$)" captured "Status: DONE"))
      (check-not-equal? projected captured "the status projection must change the live bytes")
      (check-true (regexp-match? #rx"(?m:^Status: DONE$)" projected))
      (write-text! live-path projected)
      (check-equal? (classify-snapshot-drift dir campaign-id) '())
      (delete-directory/files dir))

    (test-case "legacy v1 capture: an authored change after the failure section is refused"
      ;; Scheme 1 truncated the document at its FIRST `## Last Failure` heading,
      ;; through end of file. Dropping the failure BODY is intentional (that
      ;; section is projection-owned), but the same truncation also discards the
      ;; AUTHORED `## Verify` section that follows it — a scheme-1 over-ignore.
      ;; So a legacy digest CANNOT see this edit. Integrity keeps accepting the
      ;; recorded digest (that is what it was), and the semantic drift gate —
      ;; live projection vs AUTHENTIC snapshot projection under the CURRENT
      ;; scheme — refuses it. Neither path may resume silently.
      (define dir (make-legacy-v1-tree))
      (define authored
        (string-append (file->string legacy-v1-w0-fixture)
                       "\n## Last Failure\n\nacceptance failed\n\n"
                       "## Verify\n\nraco test EDITED.rkt\n"))
      (check-equal? (legacy-normalized-file-hash legacy-v1-w0-rel authored)
                    legacy-v1-w0-digest
                    "the legacy scheme cannot see content after the failure section")
      (check-not-equal? (current-normalized-file-hash legacy-v1-w0-rel authored) legacy-v1-w0-digest)
      (write-text! (build-path dir ".planning" legacy-v1-w0-rel) authored)
      (check-not-false (member (list legacy-v1-w0-rel 'content-drift)
                               (classify-snapshot-drift dir campaign-id))
                       "an authored change the legacy scheme hid is still drift")
      (check-exn exn:fail?
                 (lambda () (restore-plan-from-snapshot! dir campaign-id))
                 "drift is not overwritten without the explicit override")
      (check-equal? (file->string (build-path dir ".planning" legacy-v1-w0-rel)) authored)
      (delete-directory/files dir))

    (test-case "legacy v1 capture: corrupt snapshot bytes are never accepted"
      (define dir (make-legacy-v1-tree))
      (define frozen (build-path (snapshot-dir dir campaign-id) legacy-v1-w0-rel))
      ;; A same-length edit visible to BOTH schemes: refused by the loader on the
      ;; digest alone (the recorded size still matches).
      (write-text! frozen (regexp-replace #rx"## Goal" (file->string frozen) "## GOAL"))
      (check-equal? (file-size frozen) legacy-v1-w0-size)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (check-exn exn:fail? (lambda () (classify-snapshot-drift dir campaign-id)))
      (delete-directory/files dir))

    (test-case "legacy v1 capture: a tail its scheme cannot see still fails closed"
      ;; The documented limit of a v1 manifest: its digest authenticates only the
      ;; v1 projection, which truncates at the FIRST `## Last Failure`. A
      ;; same-length in-place edit after that section keeps the recorded size and
      ;; the recorded v1 digest, so the loader cannot see it — but the semantic
      ;; drift gate compares live against the frozen bytes under the CURRENT
      ;; scheme and refuses, and restore never overwrites live content.
      (define captured
        (string-append (file->string legacy-v1-w0-fixture)
                       "\n## Last Failure\n\nreason\n\n## Verify\n\nORIGINAL-TAIL\n"))
      (define dir (make-temporary-file "plan-snapshot-legacy-tail-~a" 'directory))
      (install-v1-capture! dir #:captured captured)
      (check-true (plan-snapshot-manifest? (load-snapshot-manifest dir campaign-id))
                  "the v1 digest cannot cover what its scheme discards")
      (define frozen (build-path (snapshot-dir dir campaign-id) legacy-v1-w0-rel))
      (define size-before (file-size frozen))
      (write-text! frozen (regexp-replace #rx"ORIGINAL-TAIL" (file->string frozen) "MODIFIED-TAIL"))
      (check-equal? (file-size frozen) size-before "the unauthenticated tail is edited in place")
      (check-true (plan-snapshot-manifest? (load-snapshot-manifest dir campaign-id)))
      (check-not-false (member (list legacy-v1-w0-rel 'content-drift)
                               (classify-snapshot-drift dir campaign-id))
                       "the drift gate refuses the unauthenticated tail")
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (delete-directory/files dir))

    (test-case "legacy v1 capture: symlink boundaries are still refused"
      (define dir (make-legacy-v1-tree))
      (define frozen (build-path (snapshot-dir dir campaign-id) legacy-v1-w0-rel))
      (define outside (build-path dir "outside-wave.md"))
      (write-text! outside (file->string frozen))
      (delete-file frozen)
      (make-file-or-directory-link outside frozen)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    ;; Intermediate-directory links: a symlinked `waves/` (live or frozen) is the
    ;; same trust-boundary crossing as a symlinked leaf, but `file-exists?` on
    ;; `waves/<doc>` happily resolves THROUGH it. Both directions are guarded and
    ;; pinned here because the no-follow predicate must be applied to every
    ;; component, not just the last one.
    (test-case "a symlinked live waves/ directory is a symlink boundary, not content"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define live-waves (build-path dir ".planning" "waves"))
      (define outside (build-path dir "outside-live-waves"))
      (rename-file-or-directory live-waves outside)
      (make-file-or-directory-link outside live-waves)
      ;; The leaf docs are regular files, reachable only THROUGH the link.
      (check-true (file-exists? (build-path live-waves "W0-alpha.md")))
      (check-equal? (classify-snapshot-drift dir campaign-id)
                    (list (list "waves/W0-alpha.md" 'symlink) (list "waves/W1-beta.md" 'symlink))
                    "every doc below the linked directory is symlink-classified")
      (check-exn exn:fail? (lambda () (restore-plan-from-snapshot! dir campaign-id)))
      (check-exn exn:fail?
                 (lambda ()
                   (restore-plan-from-snapshot! dir campaign-id #:override-existing-drift? #t))
                 "the symlink boundary is not overridable")
      (delete-directory/files dir))

    (test-case "a symlinked intermediate snapshot waves/ directory is refused"
      (define dir (make-legacy-v1-tree))
      (define frozen-waves (build-path (snapshot-dir dir campaign-id) "waves"))
      (define outside (build-path dir "outside-frozen-waves"))
      (rename-file-or-directory frozen-waves outside)
      (make-file-or-directory-link outside frozen-waves)
      (define frozen-leaf (build-path frozen-waves (file-name-from-path legacy-v1-w0-rel)))
      ;; The leaf still resolves to the exact captured bytes under the recorded
      ;; size and digest; only the intermediate directory is a link.
      (check-equal? (file-size frozen-leaf) legacy-v1-w0-size)
      (check-equal? (current-normalized-file-hash legacy-v1-w0-rel (file->string frozen-leaf))
                    legacy-v1-w0-current-digest)
      (check-exn exn:fail?
                 (lambda () (load-snapshot-manifest dir campaign-id))
                 "an intermediate link in the frozen tree is never resolved")
      (check-exn exn:fail? (lambda () (classify-snapshot-drift dir campaign-id)))
      (delete-directory/files dir))

    (test-case "new captures record the current scheme and verify it strictly"
      (define dir (make-plan-tree))
      (seed-and-bind-plan-snapshot! dir campaign-id)
      (define manifest (load-snapshot-manifest dir campaign-id))
      (check-equal? snapshot-hash-scheme-current 2)
      (check-equal? (plan-snapshot-manifest-hash-scheme manifest)
                    snapshot-hash-scheme-current
                    "a new capture documents its digest scheme")
      (check-equal? snapshot-hash-scheme-versions '(1 2))
      ;; The recorded wave digest is the current-scheme digest of the bytes.
      (define frozen (build-path (snapshot-dir dir campaign-id) "waves" "W0-alpha.md"))
      (check-equal? (for/first ([f (in-list (plan-snapshot-manifest-files manifest))]
                                #:when (equal? (snapshot-file-path f) "waves/W0-alpha.md"))
                      (snapshot-file-sha256 f))
                    (current-normalized-file-hash "waves/W0-alpha.md" (file->string frozen)))
      (delete-directory/files dir))

    (test-case "a declared scheme is verified strictly, with no other-scheme fallback"
      ;; The same legacy capture, but the manifest DECLARES the current scheme:
      ;; the legacy digest must then be refused instead of silently accepted.
      (define dir (make-temporary-file "plan-snapshot-strict-~a" 'directory))
      (install-v1-capture! dir #:manifest-scheme 2)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "an unsupported declared digest scheme is refused"
      (define dir (make-temporary-file "plan-snapshot-scheme-~a" 'directory))
      (install-v1-capture! dir #:manifest-scheme 3)
      (check-exn exn:fail? (lambda () (load-snapshot-manifest dir campaign-id)))
      (delete-directory/files dir))

    (test-case "seed-and-bind reuses a legacy v1 capture without re-freezing it"
      ;; Resume path: the authentic v1 snapshot is reused verbatim (never
      ;; rewritten, never re-snapshotted) and the frozen-contract check still
      ;; compares live against the frozen bytes.
      (define dir (make-legacy-v1-tree))
      (define-values (_path digest) (seed-and-bind-plan-snapshot! dir campaign-id))
      (check-equal? (plan-snapshot-manifest-hash-scheme (load-snapshot-manifest dir campaign-id))
                    #f
                    "an existing legacy capture keeps its recorded (absent) scheme")
      (check-equal? digest (snapshot-manifest-digest (load-snapshot-manifest dir campaign-id)))
      (write-text! (build-path dir ".planning" "PLAN.md")
                   (string-append legacy-v1-plan-text "\nAmended contract clause\n"))
      (check-exn exn:fail:gsd-frozen-contract-stale?
                 (lambda () (seed-and-bind-plan-snapshot! dir campaign-id)))
      (delete-directory/files dir))))

(void (run-tests plan-snapshot-suite))
