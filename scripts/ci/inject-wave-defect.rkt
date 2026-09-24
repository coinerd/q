#lang racket/base

;; v1.00.31 W6 — adversarial permanence rehearsal.
;;
;; Proves permanence for the frozen wave-delivery integrity register: the exact
;; defects that blocked v1.00.30 W4 (F1-F13) are injected into scratch fixtures
;; (never a live branch) and each row's shipped guard is run against the
;; injection. A row is `refused` only when the guard's own typed refusal is
;; observed; a row whose injection passes silently (`ok`), whose guard is absent
;; (`guard-missing`), or which is `skipped`, is a rehearsal failure.
;;
;; Two negative controls keep the rehearsal from proving nothing:
;;   * a clean synthetic wave must be accepted by the same guards (so the
;;     guards are not merely refusing everything), and
;;   * a guard file that is missing/mislocated must be detected as
;;     `guard-missing` (so a guard cannot be neutered by omission; fail closed).
;;
;; The rehearsal emits one canonical JSON matrix (sorted keys, 1-space indent,
;; trailing newline, ASCII escapes — the same canonical form the W5 provenance
;; lint enforces) bound to the W0 register digest and to the rehearsal head, so
;; a later register edit invalidates the verdict instead of silently inheriting
;; it.
;;
;; Usage:
;;   racket scripts/ci/inject-wave-defect.rkt [--root <checkout>] [--out <path>]
;;                                            [--missing-guard-dir <dir>]
(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         racket/runtime-path
         json
         (file "../../util/json/checksum.rkt")
         (file "./invocation-contract.rkt")
         (file "../gsd-evidence-bind.rkt")
         (file "../../extensions/gsd/campaign-state.rkt")
         (file "../../extensions/gsd/campaign-repository.rkt")
         (file "../../extensions/gsd/wave-completion.rkt")
         (file "../../extensions/gsd/delivery-journal.rkt")
         (file "../../extensions/gsd/delivery-receipt.rkt")
         (file "../../extensions/gsd/plan-snapshot.rkt")
         (only-in (file "../gsd-wave-gate.rkt") validate-wave-evidence wave-evidence-result-reasons))

(define-runtime-path here-path ".")
(define here (simplify-path here-path))
(define default-root (simplify-path (build-path here ".." "..")))

;; ---------------------------------------------------------------------------
;; Subprocess capture
;; ---------------------------------------------------------------------------

(define (capture argv [cwd #f])
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err]
                   [current-directory (or cwd (current-directory))])
      (with-handlers ([exn:fail? (lambda (_e) 127)])
        (apply system*/exit-code argv))))
  (values code (string-append (get-output-string out) (get-output-string err))))

(define (racket-exe)
  (or (find-executable-path "racket") "racket"))
(define (python-exe)
  (or (find-executable-path "python3") (find-executable-path "python") "python3"))
(define (git-exe)
  (or (find-executable-path "git") "git"))

(define (normalize text root)
  ;; Determinism: scratch fixtures live under temporary directories, so their
  ;; absolute spelling never reaches the matrix. The rehearsal root is scrubbed
  ;; too, so a reason never embeds a checkout path.
  (define clean
    (if (path? text)
        (path->string text)
        (format "~a" text)))
  (define scrubbed
    (regexp-replace* #rx"(?:/var/tmp|/tmp)/[^ \n|]+"
                     (regexp-replace* (regexp-quote (if (path? root)
                                                        (path->string root)
                                                        root))
                                      clean
                                      "")
                     "<scratch>"))
  (define (trim-line l)
    (string-trim l))
  (string-join (for/list ([l (in-list (string-split scrubbed "\n"))]
                          #:when (non-empty-string? (trim-line l)))
                 (trim-line l))
               " | "))

;; A `nonzero-exit+token` row is refused only when the guard BOTH fails closed
;; (non-zero exit) and names the typed refusal: a warning-mode regression that
;; prints the token and exits 0 must not be recorded as a refusal witness.
(define (token-refusal code output token)
  (and (not (zero? code)) (string-contains? output token) #t))

(define (typed-reason output token [fallback #f])
  (or (for/first ([l (in-list (string-split output "\n"))]
                  #:when (string-contains? l token))
        (string-trim l))
      fallback
      (string-trim (if (non-empty-string? (string-trim output)) output "no output"))))

;; ---------------------------------------------------------------------------
;; Row model
;; ---------------------------------------------------------------------------

;; A row reports (values exit-code reason refused?); `signal` names which
;; observation is authoritative so the matrix never hides that some shipped
;; guards print a typed verdict on exit 0 while others fail closed with a
;; non-zero exit.
;;   'nonzero-exit+token  a CLI that exits non-zero and names the typed refusal
;;   'token               a CLI that prints the typed verdict and exits 0
;;   'suite-exit0         a fixture suite that passes only after observing the
;;                        typed refusal internally (exit 0 == refused)
;;   'library             an in-process guard returning the typed outcome
(struct row (id defect guard guard-paths signal injected clean) #:transparent)

;; ---------------------------------------------------------------------------
;; Scratch fixture helpers
;; ---------------------------------------------------------------------------

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

(define (scratch-dir tag)
  (make-temporary-file (format "q-w6-~a-~a" tag "~a") 'directory))

(define (read-datum path)
  (call-with-input-file path
                        (lambda (in)
                          (parameterize ([read-accept-reader #f]
                                         [read-accept-lang #f]
                                         [read-accept-graph #f])
                            (read in)))))

;; A hermetic mini git repository for record-level injections.
(define (with-mini-repo tag proc)
  (define dir (scratch-dir tag))
  (define home (build-path dir "home"))
  (make-directory home)
  (define (git . args)
    ;; Hermetic without replacing the process environment: identity, signing and
    ;; the default branch are supplied explicitly, so a user global git config
    ;; can neither sign nor rename the fixture commits.
    (define-values (code out)
      (capture (append (list (git-exe)
                             "-c"
                             "commit.gpgsign=false"
                             "-c"
                             "user.name=CI Fixture"
                             "-c"
                             "user.email=ci@example.invalid"
                             "-c"
                             "init.defaultBranch=main")
                       args)
               dir))
    (unless (zero? code)
      (error 'inject-wave-defect "git ~a failed in ~a: ~a" args dir out))
    (string-trim out))
  ;; Fixed author/committer dates: without them every fixture commit SHA (and
  ;; therefore the typed refusal text that names it) would change per run, and
  ;; the rehearsal could not be reproduced byte-identically.
  (define env (current-environment-variables))
  (define old-author (environment-variables-ref env #"GIT_AUTHOR_DATE"))
  (define old-committer (environment-variables-ref env #"GIT_COMMITTER_DATE"))
  (define (restore!)
    (environment-variables-set! env #"GIT_AUTHOR_DATE" old-author)
    (environment-variables-set! env #"GIT_COMMITTER_DATE" old-committer))
  (environment-variables-set! env #"GIT_AUTHOR_DATE" #"2026-09-01T00:00:00Z")
  (environment-variables-set! env #"GIT_COMMITTER_DATE" #"2026-09-01T00:00:00Z")
  (dynamic-wind void
                (lambda ()
                  (git "init" "-q" "-b" "main" ".")
                  (proc dir git))
                (lambda ()
                  (restore!)
                  (delete-directory/files dir #:must-exist? #f))))

;; ---------------------------------------------------------------------------
;; F1 — declared invocation ≠ script CLI
;; ---------------------------------------------------------------------------

(define (f1-live-declaration root)
  ;; The clean control is the live declaration itself: the shipped action file
  ;; plus the shipped script, parsed by the shipped contract tool.
  (define dir (scratch-dir "f1live"))
  (define action (build-path dir ".github/actions/prepare-racket-environment/action.yml"))
  (write-text! action
               (file->string (build-path root
                                         ".github/actions/prepare-racket-environment/action.yml")))
  (write-text! (build-path dir "scripts/ci/compiled-root.rkt")
               (file->string (build-path root "scripts/ci/compiled-root.rkt")))
  (define declaration
    (for/first ([i (in-list (extract-declared-invocations action))]
                #:when (equal? (hash-ref i 'target "") "scripts/ci/compiled-root.rkt"))
      i))
  (values dir declaration))

(define (f1-status root inject?)
  (define-values (dir live) (f1-live-declaration root))
  (define declaration
    (and live
         (if inject?
             (hash-set live 'flags (cons "--w6-injected-unknown-flag" (hash-ref live 'flags '())))
             live)))
  (define status (and declaration (hash-ref (check-invocation declaration dir) 'status #f)))
  (delete-directory/files dir #:must-exist? #f)
  status)

(define (f1-injected root)
  (define status (f1-status root #t))
  (values 0 (format "invocation-contract status: ~a" status) (equal? status "unknown-flag")))

(define (f1-clean root)
  (define status (f1-status root #f))
  (values 0 (format "invocation-contract status: ~a" status) (equal? status "ok")))

;; ---------------------------------------------------------------------------
;; F2 — authored/unreproducible content digest
;; ---------------------------------------------------------------------------

(define (f2-outcome root write-bad-digest?)
  (with-mini-repo "f2"
                  (lambda (dir git)
                    (write-text! (build-path dir "src/a.rkt") "#lang racket\n")
                    (git "add" "-A" ".")
                    (git "commit" "-q" "-m" "c1 source")
                    (define base (git "rev-parse" "HEAD"))
                    (define record (build-path dir "docs/reports/gsd-wave-evidence/v.rktd"))
                    (write-text! record
                                 (format "#hasheq((content-digest . ~s))\n"
                                         (if write-bad-digest?
                                             (make-string 64 #\0)
                                             (make-string 64 #\1))))
                    (git "add" "-A" ".")
                    (git "commit" "-q" "-m" "c2 evidence")
                    (define head (git "rev-parse" "HEAD"))
                    (define path (build-path root "scripts/gsd-evidence-bind.rkt"))
                    (define-values (code output)
                      (capture (list (racket-exe)
                                     (path->string path)
                                     "verify"
                                     "--repo"
                                     dir
                                     "--base"
                                     base
                                     "--head"
                                     head
                                     "--evidence"
                                     (path->string record))))
                    (values code output))))

(define (f2-injected root)
  (define-values (code output) (f2-outcome root #t))
  (values code output (string-contains? output "digest-mismatch")))

(define (f2-clean root)
  (define-values (code output)
    (with-mini-repo "f2"
                    (lambda (dir git)
                      (write-text! (build-path dir "src/a.rkt") "#lang racket\n")
                      (git "add" "-A" ".")
                      (git "commit" "-q" "-m" "c1 source")
                      (define base (git "rev-parse" "HEAD"))
                      (define record (build-path dir "docs/reports/gsd-wave-evidence/v.rktd"))
                      (write-text! record "#hasheq((content-digest . \"PENDING\"))\n")
                      (git "add" "-A" ".")
                      (git "commit" "-q" "-m" "c2 evidence")
                      (define head (git "rev-parse" "HEAD"))
                      (define path (build-path root "scripts/gsd-evidence-bind.rkt"))
                      (define-values (_bind-code _bind-out)
                        (capture (list (racket-exe)
                                       (path->string path)
                                       "bind"
                                       "--repo"
                                       dir
                                       "--base"
                                       base
                                       "--head"
                                       head
                                       "--evidence"
                                       (path->string record))))
                      (capture (list (racket-exe)
                                     (path->string path)
                                     "verify"
                                     "--repo"
                                     dir
                                     "--base"
                                     base
                                     "--head"
                                     head
                                     "--evidence"
                                     (path->string record))))))
  (values code output (string-contains? output "digest-ok")))

;; ---------------------------------------------------------------------------
;; F3 — evidence/review head ≠ verified head
;; ---------------------------------------------------------------------------

;; One complete-trio fixture, parameterised by content quality:
;;   'sentinel  every identity and narrative field is the literal sentinel the
;;              W0 binding draft carried (F12's defect)
;;   'terse     genuine identity, narrative below the substantive minimum
;;   'genuine   valid content that the strict gate ACCEPTS (the clean control)
(define (trio-root mode)
  (define dir (scratch-dir (format "trio-~a" mode)))
  (define impl (make-string 40 #\a))
  (define other (make-string 40 #\d))
  (define digest (make-string 64 #\b))
  (define (field name)
    (if (eq? mode 'sentinel) "PENDING" name))
  (define scope
    (if (eq? mode 'terse)
        "too short"
        (make-string 70 #\s)))
  (define report
    (if (eq? mode 'terse)
        "too short"
        (make-string 90 #\r)))
  (define red-failure
    (if (eq? mode 'terse)
        "too short"
        (make-string 60 #\f)))
  (define red-command "racket scripts/ci/inject-wave-defect.rkt --row F12")
  (write-text! (build-path dir "scripts/required-pr-checks.policy") "(\"lint\")\n")
  (write-text!
   (build-path dir "docs/reports/gsd-wave-evidence/w6.rktd")
   (format (string-append "#hasheq((schema-version . 2) (milestone . 896) (wave . \"W12\")"
                          " (issue . 9731) (status . \"ready-for-merge\")"
                          " (implementation-sha . ~s) (content-digest . ~s)"
                          " (required-checks . (\"lint\"))"
                          " (review-artifact . \"docs/reports/gsd-wave-reviews/w6.rktd\")"
                          " (validation-artifact . \"docs/reports/gsd-wave-validation/w6.rktd\"))\n")
           impl
           digest))
  (write-text! (build-path dir "docs/reports/gsd-wave-reviews/w6.rktd")
               (format (string-append "#hasheq((reviewer . ~s) (verdict . \"APPROVED\")"
                                      " (reviewed-sha . ~s) (content-digest . ~s) (timestamp . ~s)"
                                      " (scope . ~s) (report . ~s))\n")
                       (field "Independent Reviewer")
                       impl
                       digest
                       (field "2026-09-23T00:00:00Z")
                       scope
                       report))
  (write-text!
   (build-path dir "docs/reports/gsd-wave-validation/w6.rktd")
   (format (string-append "#hasheq((status . \"current\") (milestone . 896) (wave . \"W12\")"
                          " (issue . 9731) (branch . \"campaign/w6\") (implementation-sha . ~s)"
                          " (content-digest . ~s) (planning-sync . \"current\")"
                          " (remaining-items . (#hasheq((classification . \"deferred-noncritical\")"
                          " (OWNER . ~s) (RATIONALE . ~s))))"
                          " (red-first . #hasheq((command . ~s) (failure . ~s)))"
                          " (focused-tests . #hasheq((result . \"passed\")))"
                          " (format-compile . #hasheq((result . \"passed\")))"
                          " (lint . #hasheq((result . \"passed\")))"
                          " (fast . #hasheq((result . \"passed\")))"
                          " (review-artifact . \"docs/reports/gsd-wave-reviews/w6.rktd\"))\n")
           impl
           digest
           (field "W6 rehearsal operator")
           (field "documents the injection instead of closing the row by assertion")
           red-command
           red-failure))
  (values dir digest impl other))

(define (trio-reasons dir digest)
  (define evidence (read-datum (build-path dir "docs/reports/gsd-wave-evidence/w6.rktd")))
  (wave-evidence-result-reasons
   (validate-wave-evidence evidence #:root dir #:actual-content-digest digest)))

(define (gate-run root dir digest receipt)
  (define gate (build-path root "scripts/gsd-wave-gate.rkt"))
  (capture (list (racket-exe)
                 (path->string gate)
                 (path->string (build-path dir "docs/reports/gsd-wave-evidence/w6.rktd"))
                 "--content-digest"
                 digest
                 "--root"
                 dir
                 "--policy"
                 (path->string (build-path dir "scripts/required-pr-checks.policy"))
                 "--receipt-head"
                 receipt)))

(define (f3-injected root)
  (define-values (dir digest impl other) (trio-root 'genuine))
  (define-values (code output) (gate-run root dir digest other))
  (delete-directory/files dir #:must-exist? #f)
  (values code output (token-refusal code output "head-binding-mismatch")))

(define (f3-clean root)
  ;; The clean control must be a wave the guard ACCEPTS — exit 0 and PASS — not
  ;; merely the absence of the injected refusal token.
  (define-values (dir digest impl _other) (trio-root 'genuine))
  (define-values (code output) (gate-run root dir digest impl))
  (delete-directory/files dir #:must-exist? #f)
  (values code output (and (zero? code) (string-contains? output "PASS"))))

;; F4 — record commit touches non-evidence paths
;; ---------------------------------------------------------------------------

(define (f4-outcome root mixed?)
  (with-mini-repo "f4"
                  (lambda (dir git)
                    (write-text! (build-path dir "src/a.rkt") "#lang racket\n")
                    (git "add" "-A" ".")
                    (git "commit" "-q" "-m" "c1 source")
                    (define base (git "rev-parse" "HEAD"))
                    (write-text! (build-path dir "docs/reports/gsd-wave-evidence/v.rktd")
                                 "#hasheq((schema-version . 2))\n")
                    (when mixed?
                      (write-text! (build-path dir "src/a.rkt") "#lang racket\n(define x 1)\n"))
                    (git "add" "-A" ".")
                    (git "commit" "-q" "-m" (if mixed? "c2 MIXED evidence+source" "c2 evidence-only"))
                    (define head (git "rev-parse" "HEAD"))
                    (define path (build-path root "scripts/gsd-evidence-bind.rkt"))
                    (capture (list (racket-exe)
                                   (path->string path)
                                   "record-commit"
                                   "--repo"
                                   dir
                                   "--base"
                                   base
                                   "--head"
                                   head)))))

(define (f4-injected root)
  (define-values (code output) (f4-outcome root #t))
  (values code output (string-contains? output "impure-record-commit")))

(define (f4-clean root)
  (define-values (code output) (f4-outcome root #f))
  (values code output (string-contains? output "pure")))

;; ---------------------------------------------------------------------------
;; F5 — wave recorded verified with an unpushed branch
;; ---------------------------------------------------------------------------

(define (f5-outcome root published?)
  (define dir (scratch-dir "f5"))
  (define plan (make-string 64 #\a))
  (define ident
    (hasheq 'repo
            "/repo"
            'branch
            "campaign/unpublished"
            'head
            (make-string 40 #\b)
            'tree
            (make-string 40 #\c)
            'origin
            "https://github.com/example/q.git"))
  (verify-with-delivery-receipt dir
                                plan
                                2
                                dir
                                (lambda () 'approved)
                                #:approved? (lambda (v) (eq? v 'approved))
                                #:evidence (lambda (_) "log")
                                #:remote-published (lambda (_r _b _h) published?)
                                #:snapshot (lambda (_) ident))
  (define journal? (and (load-delivery-journal dir plan 2) #t))
  (define marker (and (load-remote-pending dir plan 2) #t))
  (delete-directory/files dir #:must-exist? #f)
  (values 0 (format "journal=~a remote-pending=~a" journal? marker) journal? marker))

(define (f5-injected root)
  (define-values (code reason journal? marker) (f5-outcome root #f))
  (values code
          (format "~a (unpublished branch records only the typed remote-pending marker: ~a)"
                  reason
                  (and (not journal?) marker))
          (and (not journal?) (not (not marker)))))

(define (f5-clean root)
  (define-values (code reason journal? marker) (f5-outcome root #t))
  (values code
          (format "~a (published branch records the receipt and clears the marker)" reason)
          (and journal? (not marker))))

;; ---------------------------------------------------------------------------
;; F6 / F11 — GitHub API route contract / amended approval contract
;; ---------------------------------------------------------------------------

;; A fixture suite is a valid guard witness only when it actually ran and passed
;; its own assertions: exit 0 alone is vacuous (an empty suite would "refuse" by
;; passing). The witness therefore requires a `Ran N tests` line with at least
;; the row's declared minimum, an OK verdict, and every typed refusal token
;; present in the suite source — the suite must assert the refusal the row
;; claims. Both halves of each row replay the same offline suite: its failing
;; direction is the injected-defect witness, its green direction (the suite
;; asserts the correct route/authorized merge too) is the clean control.
(define suite-cache (make-hash))

(define (suite-outcome root rel min-tests tokens)
  (define path (build-path root rel))
  (define key (path->string path))
  (define result
    (hash-ref
     suite-cache
     key
     (lambda ()
       (define source (and (file-exists? path) (file->string path)))
       (define-values (code output) (capture (list (python-exe) (path->string path)) root))
       (define ran-match (regexp-match #px"Ran ([0-9]+) tests?" output))
       (define ran (and ran-match (string->number (second ran-match))))
       (define ok? (and (string-contains? output "OK") #t))
       (define missing
         (for/list ([t (in-list tokens)]
                    #:unless (and source (string-contains? source t)))
           t))
       (hash-set (hash-set (hash-set (hash-set (hash) 'code code) 'output output) 'ran (or ran 0))
                 'ok?
                 (and ok? ran (>= ran min-tests) (null? missing))))))
  (define accepted? (hash-ref result 'ok?))
  (values (hash-ref result 'code)
          (hash-ref result 'output)
          accepted?
          (format "~a test(s) ran (minimum ~a), OK=~a, suite source asserts ~a"
                  (hash-ref result 'ran)
                  min-tests
                  (and accepted? #t)
                  (string-join tokens "/"))))

(define (f6-injected root)
  (define-values (code output accepted? witness)
    (suite-outcome root
                   "tests/test-gsd-delivery-api-contract.py"
                   11
                   (list "resolve_existing_pr" "head=")))
  (values code (format "wrong {owner}/{repo} head filter: ~a" witness) accepted?))

(define (f6-clean root)
  (define-values (code output accepted? witness)
    (suite-outcome root
                   "tests/test-gsd-delivery-api-contract.py"
                   11
                   (list "resolve_existing_pr" "head=")))
  (values code
          (format "offline API-contract suite green path (correct head filter): ~a" witness)
          accepted?))

(define (f11-injected root)
  (define-values (code output accepted? witness)
    (suite-outcome root
                   "tests/test-gsd-delivery-approval-contract.py"
                   15
                   (list "no-operator-authorization" "no-review-artifact" "head-binding-mismatch")))
  (values code
          (format "absent authorization / absent APPROVED review artifact: ~a" witness)
          accepted?))

(define (f11-clean root)
  (define-values (code output accepted? witness)
    (suite-outcome root
                   "tests/test-gsd-delivery-approval-contract.py"
                   15
                   (list "no-operator-authorization" "no-review-artifact" "head-binding-mismatch")))
  (values code
          (format "offline approval-contract suite green path (recorded authorization + review): ~a"
                  witness)
          accepted?))

;; F7 — stale recorded head and cross-artifact timing disagreement
;; ---------------------------------------------------------------------------

(define (f7-injected root)
  (define scratch (scratch-dir "f7"))
  (define fixture (build-path scratch "artifacts/f7-repro/v9.99.99-w0"))
  (define gen (build-path root "artifacts/wave-delivery-integrity/v1.00.31-w5/raw/matrix-gen.py"))
  (define-values (gen-code _gen-out)
    (capture (list (python-exe)
                   (path->string gen)
                   "--emit-f7-fixture"
                   (path->string (build-path scratch "emitted")))
             root))
  (define emitted (build-path scratch "emitted/rollback-drill.json"))
  (when (zero? gen-code)
    (write-text! (build-path fixture "rollback-drill.json") (file->string emitted))
    (write-text! (build-path fixture "SHA256SUMS")
                 (format "~a  artifacts/f7-repro/v9.99.99-w0/rollback-drill.json\n"
                         (sha256-file (build-path fixture "rollback-drill.json")))))
  (define lint (build-path root "scripts/ci/verify-artifact-provenance.rkt"))
  (define-values (code output)
    (capture (list (racket-exe)
                   (path->string lint)
                   "--root"
                   (path->string scratch)
                   "--current-wave"
                   "v9.99.99-w0"
                   "--only-current-wave")))
  (delete-directory/files scratch #:must-exist? #f)
  (values code output (token-refusal code output "provenance-drift")))

(define (f7-clean root)
  (define scratch (scratch-dir "f7clean"))
  (define fixture (build-path scratch "artifacts/clean/v9.99.99-w0"))
  (write-text! (build-path fixture "matrix.json") "{\n \"count\": 1,\n \"status\": \"clean\"\n}\n")
  (write-text! (build-path fixture "SHA256SUMS")
               (format "~a  artifacts/clean/v9.99.99-w0/matrix.json\n"
                       (sha256-file (build-path fixture "matrix.json"))))
  (define lint (build-path root "scripts/ci/verify-artifact-provenance.rkt"))
  (define-values (code output)
    (capture (list (racket-exe)
                   (path->string lint)
                   "--root"
                   (path->string scratch)
                   "--current-wave"
                   "v9.99.99-w0"
                   "--only-current-wave")))
  (delete-directory/files scratch #:must-exist? #f)
  (values code output (not (string-contains? output "provenance-drift"))))

;; ---------------------------------------------------------------------------
;; F8 — opaque failure surfacing
;; ---------------------------------------------------------------------------

(define (f8-classify root line)
  (define program
    (string-append "import importlib.util as u\n"
                   "spec = u.spec_from_file_location('g', r'"
                   (path->string (build-path root "scripts" "gsd-delivery.py"))
                   "')\n"
                   "m = u.module_from_spec(spec)\nspec.loader.exec_module(m)\n"
                   "print(m.classify_failure(['git', 'fetch', 'origin', 'refs/heads/campaign/x'], "
                   "'git', 128, "
                   line
                   "))\n"))
  (define tmp (make-temporary-file "q-w6-f8-~a.py"))
  (dynamic-wind (lambda () (write-text! tmp program))
                (lambda () (capture (list (python-exe) (path->string tmp)) root))
                (lambda ()
                  (when (file-exists? tmp)
                    (delete-file tmp)))))

(define (f8-injected root)
  (define-values (code output)
    (f8-classify root "format(\"fatal: couldn't find remote ref refs/heads/campaign/x\")"))
  (values code output (string-contains? output "remote-ref-missing:")))

(define (f8-clean root)
  (define-values (code output) (f8-classify root "\"some unclassified failure\""))
  (values code
          output
          (and (not (string-contains? output "remote-ref-missing:")) (non-empty-string? output))))

;; ---------------------------------------------------------------------------
;; F9 / F10 — completion integrity and outbox two-way invariant
;; ---------------------------------------------------------------------------

(define (f9-fixture dir)
  (make-directory* (build-path dir ".planning/waves"))
  (write-text! (build-path dir ".planning/PLAN.md")
               "# Plan: W6 rehearsal\n\n## Waves\n\n- [Inbox] W0: Evidence → waves/W0-e.md\n")
  (write-text! (build-path dir ".planning/waves/W0-e.md")
               "# Wave 0\n\nGoal: e\n\n## Verify\n\nraco test .\n")
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (values rec (campaign-wave-current-attempt (car (campaign-record-waves rec)))))

(define (f9-outcome root complete?)
  (define dir (scratch-dir "f9"))
  (define-values (rec attempt) (f9-fixture dir))
  (define result
    (try-complete-wave! dir
                        rec
                        0
                        #:verifier-approve? #t
                        #:expected-attempt-id (campaign-attempt-id attempt)
                        #:expected-fence-token (campaign-attempt-fence-token attempt)
                        #:delivery-proof (if complete? 'carry-forward 'require-delivered)))
  (define status (completion-result-status result))
  (delete-directory/files dir #:must-exist? #f)
  (values 0 (format "completion-result-status: ~a" status) status))

(define (f9-injected root)
  (define-values (code reason status) (f9-outcome root #f))
  (values code reason (eq? status 'delivery-pending-cannot-complete)))

(define (f9-clean root)
  (define-values (code reason status) (f9-outcome root #t))
  (values code reason (not (eq? status 'delivery-pending-cannot-complete))))

(define (f10-outcome root rollback?)
  (define dir (scratch-dir "f10"))
  (define-values (rec attempt) (f9-fixture dir))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt))
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (when rollback?
    (set-campaign-wave-status! (car (campaign-record-waves durable)) 'pending)
    (persist-campaign! dir durable))
  (define rolled (load-campaign-record dir (campaign-plan-id rec)))
  (define leads? (completion-outbox-invariant? dir rolled))
  (reconcile-completion-outbox! dir rolled)
  (define clean? (completion-outbox-invariant? dir (load-campaign-record dir (campaign-plan-id rec))))
  (delete-directory/files dir #:must-exist? #f)
  (values 0 (format "before=~a after-reconcile=~a" leads? clean?) leads? clean?))

(define (f10-injected root)
  (define-values (code reason leads? clean?) (f10-outcome root #t))
  (values code
          (format "outbox-leads-record then reconcile → ~a (~a)" clean? reason)
          (and (eq? leads? 'outbox-leads-record) (eq? clean? 'ok))))

(define (f10-clean root)
  (define-values (code reason leads? clean?) (f10-outcome root #f))
  (values code (format "invariant ~a (~a)" leads? reason) (eq? leads? 'ok)))

;; ---------------------------------------------------------------------------
;; F12 — sentinel placeholders accepted as evidence
;; ---------------------------------------------------------------------------

(define (f12-injected root)
  ;; Injected: the sentinel trio through the strict gate (typed refusal,
  ;; non-zero exit). The terse variant pins the minimum-content rule separately,
  ;; because a placeholder is reported before the length rule can fire.
  (define-values (dir digest impl _other) (trio-root 'sentinel))
  (define-values (code output) (gate-run root dir digest impl))
  (delete-directory/files dir #:must-exist? #f)
  (define terse-reasons
    (let-values ([(terse-dir t-digest _t-impl _t-other) (trio-root 'terse)])
      (define reasons (trio-reasons terse-dir t-digest))
      (delete-directory/files terse-dir #:must-exist? #f)
      reasons))
  (define placeholder-refusal? (token-refusal code output "placeholder-evidence"))
  (define short?
    (ormap (lambda (r) (string-contains? r "insufficient-review-content")) terse-reasons))
  (values code
          (format "sentinel trio (exit ~a): ~a | terse narrative (in-process): ~a"
                  code
                  (typed-reason output "placeholder-evidence" "no placeholder reason")
                  (if short? "insufficient-review-content" "no minimum-content reason"))
          (and placeholder-refusal? (and short? #t))))

(define (f12-clean root)
  ;; Clean: the same trio with genuine content must be ACCEPTED by the strict
  ;; gate (exit 0 + PASS) and must produce neither refusal reason.
  (define-values (dir digest impl _other) (trio-root 'genuine))
  (define genuine-reasons (trio-reasons dir digest))
  (define-values (code output) (gate-run root dir digest impl))
  (delete-directory/files dir #:must-exist? #f)
  (define bad
    (filter (lambda (r)
              (or (string-contains? r "placeholder-evidence")
                  (string-contains? r "insufficient-review-content")))
            genuine-reasons))
  (values code
          (format "strict gate exit ~a, PASS=~a; placeholder/insufficient reasons: ~a"
                  code
                  (and (string-contains? output "PASS") #t)
                  (length bad))
          (and (zero? code) (string-contains? output "PASS") (null? bad))))

;; F13 — frozen contract stale while every gate reports clean
;; ---------------------------------------------------------------------------

(define (f13-outcome root amended?)
  (define dir (scratch-dir "f13"))
  (define plan "# Plan\n\n- [Inbox] W0: Alpha -> waves/W0-alpha.md\n")
  (define campaign (make-string 64 #\a))
  (write-text! (build-path dir ".planning/PLAN.md") plan)
  (write-text! (build-path dir ".planning/waves/W0-alpha.md") "# Wave 0\n\nalpha body\n")
  (define-values (_path frozen-digest) (seed-and-bind-plan-snapshot! dir campaign))
  (when amended?
    (write-text! (build-path dir ".planning/PLAN.md")
                 (string-append plan "\n## Failure Modes\n\n| F14 | body amendment |\n")))
  (define outcome
    (with-handlers ([exn:fail:gsd-frozen-contract-stale? (lambda (_e) 'frozen-contract-stale)]
                    [exn:fail? (lambda (_e) 'other-failure)])
      (seed-and-bind-plan-snapshot! dir campaign)
      'rebound))
  (define snapshot-intact?
    (and (equal? (snapshot-manifest-digest (load-snapshot-manifest dir campaign)) frozen-digest)
         (file->string (build-path (snapshot-dir dir campaign) "PLAN.md"))
         (equal? (file->string (build-path (snapshot-dir dir campaign) "PLAN.md")) plan)))
  (delete-directory/files dir #:must-exist? #f)
  (values 0
          (format "seed-and-bind outcome: ~a (frozen snapshot intact: ~a)" outcome snapshot-intact?)
          outcome
          snapshot-intact?))

(define (f13-injected root)
  (define-values (code reason outcome intact?) (f13-outcome root #t))
  (values code reason (and (eq? outcome 'frozen-contract-stale) intact?)))

(define (f13-clean root)
  (define-values (code reason outcome intact?) (f13-outcome root #f))
  (values code reason (eq? outcome 'rebound)))

;; ---------------------------------------------------------------------------
;; The register rehearsal table (F1-F13)
;; ---------------------------------------------------------------------------

;; Repository-root-relative spellings: recorded in the matrix and resolved
;; against the rehearsal root when read (so the harness works from any cwd).
(define register-path "artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json")
(define reproduction-path "artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json")

(define rows
  (list
   (row
    "F1"
    "a workflow/action declares `compiled-root.rkt build --out …`, a flag the script does not accept"
    "scripts/ci/invocation-contract.rkt (check-invocation)"
    '("scripts/ci/invocation-contract.rkt" "scripts/ci/compiled-root.rkt")
    'library
    f1-injected
    f1-clean)
   (row "F2"
        "an evidence record carries a hand-authored digest that the tool never recomputed"
        "scripts/gsd-evidence-bind.rkt verify (recompute-and-compare at the exact head)"
        '("scripts/gsd-evidence-bind.rkt")
        'token
        f2-injected
        f2-clean)
   (row "F3"
        "the evidence implementation-sha differs from the durable receipt head"
        "scripts/gsd-wave-gate.rkt (strict trio gate, head binding)"
        '("scripts/gsd-wave-gate.rkt")
        'nonzero-exit+token
        f3-injected
        f3-clean)
   (row "F4"
        "the evidence record commit also touches a non-evidence source path"
        "scripts/gsd-evidence-bind.rkt record-commit (evidence-only purity)"
        '("scripts/gsd-evidence-bind.rkt")
        'token
        f4-injected
        f4-clean)
   (row "F5"
        "a wave records verified while its branch was never pushed"
        "delivery-receipt verify-with-delivery-receipt (remote backing precondition)"
        '("extensions/gsd/delivery-receipt.rkt")
        'library
        f5-injected
        f5-clean)
   (row "F6"
        "PR resolution uses the head={owner}/{repo}:{branch} filter, which matches nothing"
        "tests/test-gsd-delivery-api-contract.py (offline API contract fixtures)"
        '("tests/test-gsd-delivery-api-contract.py")
        'suite-exit0
        f6-injected
        f6-clean)
   (row "F7"
        "an artifact pins a stale recorded head and contradicts its own structured timing"
        "scripts/ci/verify-artifact-provenance.rkt (provenance/determinism lint)"
        '("scripts/ci/verify-artifact-provenance.rkt")
        'nonzero-exit+token
        f7-injected
        f7-clean)
   (row "F8"
        "a delivery subprocess failure surfaces as a bare exit code instead of a typed reason"
        "gsd-delivery classify_failure (typed failure classification)"
        '("scripts/gsd-delivery.py")
        'library
        f8-injected
        f8-clean)
   (row "F9"
        "a wave is marked done while its delivery journal reads delivery-pending"
        "wave-completion try-complete-wave! #:delivery-proof 'require-delivered"
        '("extensions/gsd/wave-completion.rkt")
        'library
        f9-injected
        f9-clean)
   (row "F10"
        "a rolled-back wave leaves its completion event leading the durable record"
        "wave-completion completion-outbox-invariant? + reconcile-completion-outbox!"
        '("extensions/gsd/wave-completion.rkt")
        'library
        f10-injected
        f10-clean)
   (row "F11"
        "a merge is attempted with no recorded operator authorization or no APPROVED review artifact"
        "tests/test-gsd-delivery-approval-contract.py (amended approval contract fixtures)"
        '("tests/test-gsd-delivery-approval-contract.py")
        'suite-exit0
        f11-injected
        f11-clean)
   (row
    "F12"
    "a finalized-looking trio carries sentinel placeholders in its identity and narrative fields"
    "scripts/gsd-wave-gate.rkt CLI (placeholder-evidence, non-zero exit) plus the in-process minimum-content half for a terse narrative"
    '("scripts/gsd-wave-gate.rkt")
    'nonzero-exit+token
    f12-injected
    f12-clean)
   (row "F13"
        "the authored plan body is amended while the frozen snapshot still reports clean"
        "extensions/gsd/plan-snapshot.rkt seed-and-bind-plan-snapshot! (frozen-contract-stale)"
        '("extensions/gsd/plan-snapshot.rkt")
        'library
        f13-injected
        f13-clean)))

;; The rehearsal inputs whose digests bind the verdict to CONTENT, not merely
;; to a commit: any edit to the harness, either test, the register, the
;; reproduction artifact or the generators invalidates the recorded verdict
;; until the matrix is regenerated.
(define rehearsal-inputs
  (list "scripts/ci/inject-wave-defect.rkt"
        "tests/test-wave-integrity-adversarial.rkt"
        "tests/test-wave-delivery-integrity-register.rkt"
        "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/extend-register-f12-f13.py"
        "artifacts/wave-delivery-integrity/v1.00.31-w6/raw/injection-matrix-gen.py"
        "artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json"
        "artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json"))

(define (inputs-digest root)
  (sha256-string (string-join (for/list ([rel (in-list rehearsal-inputs)])
                                (format "~a ~a" rel (sha256-file (root-path root rel))))
                              "\n")))

(define (row-outcome root r)
  (define missing
    (for/list ([p (in-list (row-guard-paths r))]
               #:unless (file-exists? (build-path root p)))
      p))
  (cond
    [(pair? missing)
     (hasheq 'id
             (row-id r)
             'guard
             (row-guard r)
             'signal
             (row-signal r)
             'injected-defect
             (row-defect r)
             'exit-code
             #f
             'reason
             (format "guard file(s) missing under the rehearsal root: ~a" (string-join missing ", "))
             'outcome
             "guard-missing")]
    [else
     (define-values (code reason refused?) ((row-injected r) root))
     (hasheq 'id
             (row-id r)
             'guard
             (row-guard r)
             'signal
             (row-signal r)
             'injected-defect
             (row-defect r)
             'exit-code
             code
             'reason
             (normalize reason root)
             'outcome
             (if refused? "refused" "ok"))]))

(define (row-outcomes [root default-root])
  (for/list ([r (in-list rows)])
    (row-outcome root r)))

(define (control-outcome root r)
  (define-values (code reason accepted?) ((row-clean r) root))
  (hasheq 'id
          (row-id r)
          'guard
          (row-guard r)
          'exit-code
          code
          'reason
          (normalize reason root)
          'outcome
          (if accepted? "accepted" "refused")))

;; ---------------------------------------------------------------------------
;; Verdict, binding and canonical JSON
;; ---------------------------------------------------------------------------

(define (read-json-file path)
  (with-input-from-file path (lambda () (read-json))))

(define (root-path root rel)
  (build-path root rel))

(define (file-digest root rel)
  (sha256-file (root-path root rel)))

(define (git-head root)
  (define-values (code out) (capture (list (git-exe) "rev-parse" "HEAD") root))
  (and (zero? code) (string-trim out)))

(define (commit-ancestor? root ancestor descendant)
  (and ancestor
       descendant
       (let-values ([(code _out)
                     (capture (list (git-exe) "merge-base" "--is-ancestor" ancestor descendant)
                              root)])
         (zero? code))))

(define (rehearsal-head root committed)
  ;; A committed artifact cannot carry its own commit hash: the rehearsal head
  ;; is a pinned observation (the head at first generation, kept stable so
  ;; repeated rehearsals reproduce byte-identically) that SELF-HEALS when it is
  ;; no longer an ancestor of the current tip, so a later commit cannot silently
  ;; inherit a stale head. The content binding (rehearsal-inputs-digest) is the
  ;; stronger anti-inheritance mechanism.
  (define pinned (and committed (hash-ref committed 'rehearsal-head #f)))
  (define current (git-head root))
  (if (and (string? pinned)
           (regexp-match? #px"^[0-9a-f]{40}$" pinned)
           (commit-ancestor? root pinned current))
      pinned
      (or current "unknown")))

(define (canonical-json v)
  (define (escape s)
    (define out (open-output-string))
    (for ([ch (in-string s)])
      (cond
        [(char=? ch #\") (display "\\\"" out)]
        [(char=? ch #\\) (display "\\\\" out)]
        [(char=? ch #\newline) (display "\\n" out)]
        [(char=? ch #\return) (display "\\r" out)]
        [(char=? ch #\tab) (display "\\t" out)]
        [(char=? ch #\backspace) (display "\\b" out)]
        [(char=? ch #\page) (display "\\f" out)]
        [(< (char->integer ch) 32)
         (display (format "\\u~a"
                          (string-downcase (~a (number->string (char->integer ch) 16)
                                               #:min-width 4
                                               #:pad-string "0")))
                  out)]
        [(< (char->integer ch) 127) (write-char ch out)]
        [(<= (char->integer ch) 65535)
         (display (format "\\u~a"
                          (string-downcase (~a (number->string (char->integer ch) 16)
                                               #:min-width 4
                                               #:pad-string "0")))
                  out)]
        [else
         (define n (- (char->integer ch) #x10000))
         (display (format "\\u~a\\u~a"
                          (string-downcase (~a (number->string (+ #xd800 (quotient n 1024)) 16)
                                               #:min-width 4
                                               #:pad-string "0"))
                          (string-downcase (~a (number->string (+ #xdc00 (remainder n 1024)) 16)
                                               #:min-width 4
                                               #:pad-string "0")))
                  out)]))
    (get-output-string out))
  (define (render v indent)
    (define pad (make-string indent #\space))
    (define pad* (make-string (add1 indent) #\space))
    (cond
      [(hash? v)
       (define keys
         (sort (for/list ([k (in-hash-keys v)])
                 (format "~a" k))
               string<?))
       (if (null? keys)
           "{}"
           (string-append
            "{\n"
            (string-join
             (for/list ([k keys])
               (format "~a~s: ~a" pad* k (render (hash-ref v (string->symbol k)) (add1 indent))))
             ",\n")
            "\n"
            pad
            "}"))]
      [(list? v)
       (if (null? v)
           "[]"
           (string-append "[\n"
                          (string-join (for/list ([x (in-list v)])
                                         (string-append pad* (render x (add1 indent))))
                                       ",\n")
                          "\n"
                          pad
                          "]"))]
      [(string? v) (string-append "\"" (escape v) "\"")]
      [(boolean? v) (if v "true" "false")]
      [(not v) "null"]
      [(exact-integer? v) (format "~a" v)]
      [else (string-append "\"" (escape (format "~a" v)) "\"")]))
  (string-append (render v 0) "\n"))

(define (run-rehearsal [root default-root] #:missing-guard-root [missing-root #f])
  (define committed
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (with-input-from-file
       (build-path root "artifacts/wave-delivery-integrity/v1.00.31-w6/injection-matrix.json")
       (lambda () (read-json)))))
  (define head (rehearsal-head root committed))
  (define register-json (read-json-file (root-path root register-path)))
  (define active-root (or missing-root root))
  (define outcomes
    (for/list ([r (in-list rows)])
      (row-outcome active-root r)))
  (define refused
    (for/list ([o (in-list outcomes)]
               #:when (equal? (hash-ref o 'outcome) "refused"))
      (hash-ref o 'id)))
  (define failing
    (for/list ([o (in-list outcomes)]
               #:unless (equal? (hash-ref o 'outcome) "refused"))
      (hash-ref o 'id)))
  (define controls
    (for/list ([r (in-list rows)])
      (control-outcome root r)))
  (define clean-ok? (andmap (lambda (c) (equal? (hash-ref c 'outcome) "accepted")) controls))
  ;; Missing-guard control: run every row against a root that lacks the guard
  ;; files. The row model refuses before executing anything, so the rehearsal
  ;; reports guard-missing for every row instead of silently skipping -- a guard
  ;; cannot be neutered by omission, and the control costs no subprocesses.
  (define missing-dir (or missing-root (make-temporary-file "q-w6-missing-~a" 'directory)))
  (define missing-outcomes
    (for/list ([r (in-list rows)])
      (row-outcome missing-dir r)))
  (define missing-guard-detected?
    (andmap (lambda (o) (equal? (hash-ref o 'outcome) "guard-missing")) missing-outcomes))
  (unless missing-root
    (delete-directory/files missing-dir #:must-exist? #f))
  (define verdict
    (if (and (null? failing) clean-ok? missing-guard-detected?) "PERMANENT" "NOT PERMANENT"))
  (hasheq 'schema
          "wave-integrity-injection-matrix/1"
          'plan-id
          "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75"
          'milestone
          896
          'wave
          "W6"
          'purpose
          (string-append "Rehearsal of every frozen register row F1-F13: each defect is injected "
                         "into a scratch fixture and the shipped guard must refuse it with its typed "
                         "reason. PERMANENT requires all 13 rows refused, the clean synthetic wave "
                         "accepted by every guard, and a missing guard detected instead of skipped.")
          'rehearsal-head
          head
          'rehearsal-inputs
          (for/list ([rel (in-list rehearsal-inputs)])
            (hasheq 'path rel 'sha256 (sha256-file (root-path root rel))))
          'rehearsal-inputs-digest
          (inputs-digest root)
          'register
          (hasheq 'path
                  register-path
                  'sha256
                  (file-digest root register-path)
                  'row-count
                  (length (hash-ref register-json 'rows))
                  'reproduction-path
                  reproduction-path
                  'reproduction-sha256
                  (file-digest root reproduction-path))
          'rows
          outcomes
          'controls
          (hasheq 'clean-synthetic-wave
                  (hasheq 'outcome (if clean-ok? "accepted" "refused") 'per-row controls)
                  'missing-guard
                  (hasheq 'outcome
                          (if missing-guard-detected? "detected" "not-tested")
                          'detail
                          (string-append "every row reports guard-missing (fail closed) when the "
                                         "guard files are absent from the rehearsal root")))
          'refused-rows
          refused
          'failing-rows
          failing
          'verdict
          verdict))

(define (parse-args args)
  (let loop ([args args]
             [root default-root]
             [out #f]
             [missing #f])
    (cond
      [(null? args) (values root out missing)]
      [(equal? (car args) "--root") (loop (cddr args) (cadr args) out missing)]
      [(equal? (car args) "--out") (loop (cddr args) root (cadr args) missing)]
      [(equal? (car args) "--missing-guard-dir") (loop (cddr args) root out (cadr args))]
      [else (error 'inject-wave-defect "unknown argument: ~a" (car args))])))

(define (main args)
  (define-values (root out missing) (parse-args args))
  (define matrix (run-rehearsal root #:missing-guard-root missing))
  (define rendered (canonical-json matrix))
  (if out
      (begin
        (write-text! out rendered)
        (display rendered))
      (display rendered))
  (if (equal? (hash-ref matrix 'verdict) "PERMANENT") 0 1))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))

(provide run-rehearsal
         canonical-json
         token-refusal
         inputs-digest
         rehearsal-inputs
         row-outcomes
         rows
         row-id
         register-path
         reproduction-path)
