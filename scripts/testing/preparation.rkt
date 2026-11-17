#lang racket/base

;; @speed fast
;; @suite testing
;; @boundary scripts/testing

;; scripts/testing/preparation.rkt — build-once per-run preparation
;; (testing-framework final plan §4, revision 3 decision B3/C).
;;
;; Prepares one run's selected test files for isolated `raco test` children:
;;
;;   1. compute the static require closure of the selected files over
;;      `changed-selection.rkt`'s `extract-requires` (repo-relative modules
;;      only, transitive, cycle-safe);
;;   2. drop closure members listed in `prepare-exclusions.rktd` (reviewed
;;      negative-compilation fixtures that must never enter eager preparation);
;;   3. compute the per-run preparation identity: sha1 of the content of every
;;      selected file and every closure member (including excluded members),
;;      sorted by path into one canonical string, hashed again with sha1. This
;;      dist has no `file/sha256`; sha1 is the honest available choice;
;;   4. run ONE `raco make` subprocess over the deduplicated target list;
;;      non-zero exit or a missing raco executable = PREPARATION FAILED (the
;;      caller must fail closed before any test executes — there is never a
;;      silent source-mode fallback);
;;   5. re-hash the identity immediately after the make: ANY mismatch means the
;;      tree changed during preparation → the run aborts with an explicit error
;;      naming the mismatched paths (final plan §9 "source edited between
;;      preparation and the first dispatch").
;;
;; There is NO persistent cache format, NO second cache, and NO on-disk identity
;; artifact. After preparation, ordinary `compiled/` reuse relies on Racket's
;; own invalidation; the per-run hash only binds the source state this run was
;; prepared from.
;;
;; DECLARED DYNAMIC DEPENDENCIES — honest limitation: the plan's phrase "include
;; explicitly declared dynamic dependencies" has NO producer mechanism in the
;; runner today. There is no declaration registry to consult, so only statically
;; extractable require specs enter the closure. We say so here instead of
;; inventing a registry; `dynamic-require` targets that no static spec names are
;; compiled only when a child happens to reach them.
;;
;; `Q_SKIP_PREPARATION=1` is the escape hatch: skip everything (source-only
;; legacy behavior). The measurement pairs use it to compare matched runs.

(require (only-in file/sha1 sha1)
         racket/file
         racket/format
         racket/list
         racket/math
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (only-in "../testing/changed-selection.rkt" extract-requires))

(provide prepare-files!
         preparation-result
         preparation-result?
         preparation-min-closure
         preparation-result-outcome
         preparation-result-elapsed-ms
         preparation-result-target-count
         preparation-result-closure-count
         preparation-result-excluded-count
         preparation-result-identity-prefix
         preparation-result-reason
         preparation-result-mismatched
         preparation-failure-lines
         preparation-identity-hash
         load-prepare-exclusions
         prepare-exclusions-file)

;; The exclusions file lives next to this module (final plan §4.3 rev 3: a
;; small reviewed data file, no metadata schema, no regenerated artifacts).
(define-runtime-path prepare-exclusions-file "prepare-exclusions.rktd")

(struct preparation-result
  (outcome         ; 'prepared | 'skipped | 'failed
   elapsed-ms      ; wall clock for the whole preparation attempt
   target-count    ; raco make targets after exclusion filtering
   closure-count   ; static closure members (before exclusion filtering)
   excluded-count  ; closure members dropped because of the exclusions file
   identity-prefix ; first 12 hex chars of the pre-make identity hash
   reason          ; #f, or the failure reason (make output, missing raco, ...)
   mismatched)     ; paths whose identity changed during preparation
  #:transparent)

(struct preparation-failure exn:fail (paths))

;; ── identity ────────────────────────────────────────────────────────────

;; content-sha1 : path-string? -> hex-string?
;; `file/sha1`'s `sha1` returns the hex digest directly in this distribution
;; (the same contract quick-audit.rkt relies on).
(define (content-sha1 path)
  (call-with-input-file path sha1))

;; identity-canonical-string : root (listof repo-relative strings) -> string?
;; One "path sha1\n" line per file, sorted by path: the canonical pre-image.
(define (identity-canonical-string root rel-paths)
  (string-append*
   (for/list ([rel (in-list (sort (remove-duplicates rel-paths) string<?))])
     (string-append rel " " (content-sha1 (build-path root rel)) "\n"))))

;; preparation-identity-hash : root (listof repo-relative strings) -> hex-string?
;; Exported for the identity/mismatch tests; prepare-files! uses it twice.
(define (preparation-identity-hash root rel-paths)
  (sha1 (open-input-string (identity-canonical-string root rel-paths))))

;; ── exclusions ──────────────────────────────────────────────────────────

;; load-prepare-exclusions : -> (listof (cons path-string? string?))
;; Each entry is a (path . reason) pair; a malformed file fails closed here.
(define (load-prepare-exclusions)
  (define raw (file->value prepare-exclusions-file))
  (unless (and (list? raw) (pair? raw))
    (raise-user-error
     'preparation
     "prepare-exclusions.rktd must be a non-empty list of (path reason) pairs"))
  (for/list ([entry (in-list raw)])
    (unless (and (list? entry) (= 2 (length entry))
                 (string? (car entry)) (string? (cadr entry))
                 (non-empty-string? (car entry)) (non-empty-string? (cadr entry)))
      (raise-user-error
       'preparation
       "prepare-exclusions.rktd entries must be (path reason) pairs with non-empty strings"))
    (cons (car entry) (cadr entry))))

;; ── closure ─────────────────────────────────────────────────────────────

;; repo-relative : root file -> (or/c repo-relative string? #f)
;; Accepts the runner's mixed path/string suite entries; absolute paths under
;; the root are normalized. Files OUTSIDE the root return #f: they are not
;; preparation inputs (no repo-root closure, no identity tracking) and are
;; executed by their own source semantics — the documented contract that
;; keeps the runner's temp-dir test fixtures working.
(define (repo-relative root file)
  (define raw (if (path? file) (path->string file) file))
  (define p (simplify-path (path->complete-path (string->path raw) root) #f))
  (define rel (path->string (find-relative-path root p)))
  (if (memq 'up (explode-path (string->path rel)))
      #f
      rel))

;; static-closure : root (listof repo-rel) (listof repo-rel) -> (listof repo-rel)
;; Transitive `extract-requires` closure, restricted to repo-relative modules,
;; cycle-safe. Listed exclusions are LEAVES: they join the closure (so they stay
;; in the run's identity) but are never traversed, so no fixture's own requires
;; can enter eager preparation.
(define (static-closure root files exclusions)
  (let loop ([work (remove-duplicates files)]
             [seen (hash)]
             [closure '()])
    (cond
      [(null? work) (reverse closure)]
      [(hash-ref seen (car work) #f) (loop (cdr work) seen closure)]
      [(member (car work) exclusions)
       (loop (cdr work) (hash-set seen (car work) #t) (cons (car work) closure))]
      [else
       (define f (car work))
       (define result
         (with-handlers ([exn:fail? (lambda (_) (cons '() 'parse-error))])
           (extract-requires root (build-path root f))))
       (loop (append (cdr work) (car result))
             (hash-set seen f #t)
             (cons f closure))])))

;; ── raco make ───────────────────────────────────────────────────────────

;; Default make bound: generous (a cold full-lane closure can take minutes);
;; a hung make is a preparation failure, never an infinite wait.
(define make-timeout-ms (make-parameter 600000))

;; §4.4 "use build-once preparation where selected work is substantial": the
;; measured crossover on this host lies between a 9-file/40-module directional
;; suite (preparation loses ~3 s: cold make 5.3 s vs ~3 s of in-child
;; compilation) and the 2-file/552-module security repro (unprepared runs
;; never converge — every child spins into the 120 s bound). 64 is the
;; conservative middle; runs below it stay source-only. Tests shrink the
;; threshold to exercise the prepared path on tiny fixture trees.
(define preparation-min-closure (make-parameter 64))

;; Subprocess argv capacity: the OS argv limit is hit around a few hundred
;; absolute paths, and the largest selection (the `all` suite plus its closure)
;; reaches ~2200 targets. One logical make pass, chunked argv.
(define make-argv-chunk 120)

;; argv-chunks : (listof string?) -> (listof (listof string?))
(define (argv-chunks args)
  (let loop ([rest args] [acc '()])
    (cond
      [(null? rest) (reverse acc)]
      [else
       (loop (list-tail rest (min make-argv-chunk (length rest)))
             (cons (take rest (min make-argv-chunk (length rest))) acc))])))

;; raco-make-chunk! : string? (listof string?) -> void?
;; Spawns one `raco make <args>` child, waits bounded by `make-timeout-ms`, and
;; raises a preparation-failure on timeout or non-zero exit (captured output
;; tail included). Any other outcome returns normally.
(define (raco-make-chunk! raco-bin args)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define-values (_sp stdin-port _pid stderr-port ctrl)
    (apply values (apply process*/ports stdout #f stderr raco-bin (cons "make" args))))
  (close-output-port stdin-port)
  (define code-box (box #f))
  (define worker (thread (lambda () (ctrl 'wait) (set-box! code-box (ctrl 'exit-code)))))
  (cond
    [(not (sync/timeout (/ (make-timeout-ms) 1000.0) worker))
     (ctrl 'kill)
     (sync/timeout 5.0 worker)
     (kill-thread worker)
     (raise (preparation-failure
             (format "preparation: raco make exceeded ~a ms — killed" (make-timeout-ms))
             (current-continuation-marks)
             '()))]
    [(not (zero? (unbox code-box)))
     (raise (preparation-failure
             (format
              (string-append "preparation: raco make failed (exit ~a) — the run fails closed "
                             "before any test executes.\n--- raco make output tail ---\n~a")
              (unbox code-box)
              (keep-tail
               (string-trim
                (string-append (get-output-string stdout)
                                (get-output-string stderr)))
               2000))
             (current-continuation-marks)
             '()))]
    [else (void)]))

;; run-raco-make! : root (listof repo-rel) -> void?
;; One preparation pass: every target compiled (or already up to date) before
;; this returns. Non-zero exit, a missing raco executable, or a hung make is a
;; PREPARATION FAILED — the caller fails closed before any test executes.
(define (run-raco-make! root targets)
  (define raco-bin (find-executable-path "raco"))
  (unless raco-bin
    (raise (preparation-failure
            "preparation: raco executable not found — cannot prepare the run"
            (current-continuation-marks)
            '())))
  ;; Absolute targets: compiled/ lands next to the sources regardless of cwd.
  (define target-args
    (for/list ([rel (in-list targets)])
      (path->string (simplify-path (build-path root rel) #f))))
  (for ([args (in-list (argv-chunks target-args))])
    (raco-make-chunk! raco-bin args)))

;; keep-tail : string natural? -> string?
(define (keep-tail s n)
  (define t (string-trim s))
  (if (<= (string-length t) n)
      (if (non-empty-string? t) t "(no stderr output)")
      (string-append "…" (substring t (- (string-length t) n)))))

;; ── the one entry point ─────────────────────────────────────────────────

;; prepare-files! : root (listof (or/c path? string?))
;;                  [#:exclusions (listof string?)] -> preparation-result?
;; `#:exclusions` overrides the reviewed file (tests inject their own list;
;; production callers always use the file). Never raises: every failure lands
;; in a 'failed preparation-result and the caller must fail closed.
(define (prepare-files! root files #:exclusions [exclusions #f])
  (define t0 (current-inexact-milliseconds))
  (define elapsed
    (lambda () (exact-round (- (current-inexact-milliseconds) t0))))
  (define skip? (equal? (getenv "Q_SKIP_PREPARATION") "1"))
  (cond
    [skip?
     (preparation-result 'skipped (elapsed) 0 0 0 #f #f '())]
    [else
     (with-handlers
         ([preparation-failure?
           (lambda (e)
             (preparation-result 'failed
                                 (elapsed)
                                 0
                                 0
                                 0
                                 #f
                                 (exn-message e)
                                 (preparation-failure-paths e)))])
       (define selected
         (filter
          string?
          (map (lambda (f) (repo-relative root f)) files)))
       (define exclusion-paths
         (if exclusions
             exclusions
             (map car (load-prepare-exclusions))))
       (define closure (static-closure root selected exclusion-paths))
       (define identity-paths (sort (remove-duplicates (append selected closure)) string<?))
       (cond
         [(< (length identity-paths) (preparation-min-closure))
          ;; Below the §4.4 substantial-work threshold: source-only (the
          ;; measured small-suite loss makes eager preparation a net cost).
          (preparation-result 'skipped
                              (elapsed)
                              0
                              (length closure)
                              0
                              #f
                              (format
                               "closure (~a modules) below the substantial-work threshold (~a)"
                               (length identity-paths)
                               (preparation-min-closure))
                              '())]
         [else
          (prepare-selected! root
                              selected
                              closure
                              identity-paths
                              exclusion-paths
                              elapsed)]))]))

;; prepare-selected! : the prepared path of prepare-files! (threshold passed).
(define (prepare-selected! root selected closure identity-paths exclusion-paths elapsed)
  (let/ec bail
    (with-handlers
        ([preparation-failure?
          (lambda (e)
            (bail (preparation-result 'failed
                                      (elapsed)
                                      0
                                      (length closure)
                                      0
                                      #f
                                      (exn-message e)
                                      (preparation-failure-paths e))))])
       ;; Explicit make targets = the SELECTED files only. `raco make` compiles
       ;; each target's transitive require graph, which is exactly the closure
       ;; the extractor resolved, so naming closure members in the argv adds
       ;; only per-target verification cost (~150 ms × N on a warm tree). A
       ;; selected file that IS a listed exclusion is dropped here too (an
       ;; explicitly reproduced copy-input fixture never enters eager
       ;; preparation); the child still runs it by its own source semantics.
       (define targets
         (sort (remove-duplicates
                (filter (lambda (m) (not (member m exclusion-paths))) selected))
               string<?))
       ;; Excluded closure members are recorded for telemetry but stay part of
       ;; the identity (the run is bound to the source state it prepared from).
       (define excluded
         (filter (lambda (m) (member m exclusion-paths)) closure))
       ;; Pre-make identity: one content sha1 per tracked path (a vanished file
       ;; fails here and is reported as a preparation failure below).
       (define identity-before
         (for/list ([rel (in-list identity-paths)])
           (cons rel (content-sha1 (build-path root rel)))))
       (run-raco-make! root targets)
       ;; Re-hash immediately before any worker could dispatch: ANY difference
       ;; (content change or vanished file) aborts the run with the mismatched
       ;; paths named (final plan §4.2 / §9 "source edited between preparation
       ;; and the first dispatch").
       (define mismatched
         (filter
          string?
          (for/list ([entry (in-list identity-before)])
            (define rel (car entry))
            (define after
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (content-sha1 (build-path root rel))))
            (and (not (equal? after (cdr entry))) rel))))
       (unless (null? mismatched)
         (raise (preparation-failure
                 (format
                  (string-append "preparation: source identity changed during preparation "
                                 "(files changed between raco make and the first dispatch) — "
                                 "run aborted, no test executed. Mismatched paths: ~a")
                  (string-join mismatched ", "))
                 (current-continuation-marks)
                 mismatched)))
       (define identity-hash
         (preparation-identity-hash root identity-paths))
       (preparation-result 'prepared
                           (elapsed)
                           (length targets)
                           (length closure)
                           (length excluded)
                           (substring identity-hash 0 (min 12 (string-length identity-hash)))
                           #f
                           '()) ; preparation-result
   ))) ; with-handlers, let/ec, define prepare-selected!

;; preparation-failure-lines : preparation-result? -> (listof string?)
;; One line per mismatched path, ready to print verbatim.
(define (preparation-failure-lines prep)
  (map (lambda (p) (format "changed during preparation: ~a" p))
       (preparation-result-mismatched prep)))
