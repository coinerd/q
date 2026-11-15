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
         (file "../../extensions/gsd/delivery-handoff.rkt")
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

;; "pure" is a substring of "impure-record-commit": a success verdict must be
;; matched exactly, never by substring, or a guard that refuses everything would
;; satisfy its own clean control.
;; The evidence-bind CLI and the classifier report DECIDED verdicts only on exit
;; 0; a non-zero exit is a tool failure, so a crash that prints the verdict text
;; must not count as a refusal or a success.
;; A decided verdict is a verdict LINE, not a substring anywhere in the output:
;; `forbidden` names lines (by prefix) that must be absent, so a guard printing
;; both the refusal and the success verdict is not treated as DECIDED.
(provide decided-verdict?
         verdict-line?
         pure-verdict?)

(define (verdict-line? output token)
  (for/or ([l (in-list (string-split output "\n"))])
    (string-prefix? (string-trim l) token)))

(define (decided-verdict? code output token [forbidden '()])
  (and (zero? code)
       (verdict-line? output token)
       (not (for/or ([f (in-list forbidden)])
              (verdict-line? output f)))
       #t))

(define (pure-verdict? output)
  (and (not (string-contains? output "impure")) (equal? (string-trim output) "pure")))

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
;;   'suite-declares      a native owner suite whose DECLARED cases (count + the
;;                        typed refusal vocabulary they carry) are the witness:
;;                        the row is refused only when every owner still
;;                        declares them, and a mutated copy must go red
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
  (values code output (decided-verdict? code output "digest-mismatch" (list "digest-ok"))))

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
  (values code output (decided-verdict? code output "digest-ok" (list "digest-mismatch"))))

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
  (values code output (decided-verdict? code output "impure-record-commit" (list "pure"))))

(define (f4-clean root)
  (define-values (code output) (f4-outcome root #f))
  ;; "pure" is a substring of "impure-record-commit": the clean control must
  ;; observe the exact pure verdict, not the injected refusal's text.
  (values code output (and (zero? code) (pure-verdict? output))))

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

;; Phase B deleted the two offline Python contract suites that first guarded
;; these rows; their native RackUnit ports are the owners now:
;;   F6  — tests/test-gsd-delivery-api-native.rkt (API READ layer: routes,
;;         memoized reads, typed refusals) and
;;         tests/test-gsd-delivery-read-ladder.rkt (the read ladder that resolves
;;         PRs through the real `head={owner}:{branch}` filter);
;;   F11 — tests/test-gsd-delivery-merge.rkt (recorded operator authorization,
;;         the non-author APPROVED review, and their typed refusals).
;;
;; The witness keeps the row's original semantics: a suite that declares the
;; row's cases IS the refusal witness (an empty or gutted suite must not be able
;; to "refuse" by declaring nothing), so it pins three SOURCE facts per owner —
;; the file exists, it still declares at least the recorded number of
;; `test-case` forms, and it still names the typed refusal vocabulary the row
;; refuses by. A rename or a dropped case that carried the token therefore goes
;; red here. The owners' own green run is gated by the fast suite on every
;; change and is not re-executed here: together they are ~7.5 min of real-Git
;; fixture work, which is a second wall clock for the same witness.
;;
;; Both halves replay the same contract. The injected half hands the witness a
;; MUTATED COPY of each owner in a scratch root — the two mutations the Python
;; fixtures carried: a refusal string changed (the route / typed reason the row
;; is made of is no longer named) and a case dropped — so the witness must go
;; red on its own terms; the clean half checks the real owners and must go
;; green.
(struct suite-owner (rel min-cases tokens) #:transparent)

(define f6-owners
  (list (suite-owner "tests/test-gsd-delivery-api-native.rkt"
                     46
                     '("pulls?state=all" "exn:fail:delivery-action"))
        (suite-owner "tests/test-gsd-delivery-read-ladder.rkt"
                     41
                     '("head=coinerd:" "pulls?state=open" "resolve-existing-pr"))))

(define f11-owners
  (list (suite-owner "tests/test-gsd-delivery-merge.rkt"
                     10
                     '("merge-authorization refuses every under-specified direction"
                       "reviewed-head-approval refuses a non-approved, anonymous or misbound review"
                       "no-operator-authorization"
                       "no-review-artifact"
                       "head-binding-mismatch"))))

(define suite-owner-cache (make-hash))

;; Declared `test-case` forms: `raco fmt` puts each form at the start of its own
;; line, so the line-level count is exact and needs no reader (the owners are
;; `#lang` modules whose cases live in a `module+ test` submodule).
(define (declared-case-count source)
  (for/fold ([n 0]) ([line (in-list (string-split source "\n"))])
    (if (string-prefix? (string-trim line) "(test-case")
        (add1 n)
        n)))

;; owner-outcome : path? suite-owner? -> (hasheq 'rel 'witnessed? 'detail)
(define (owner-outcome root spec)
  (define rel (suite-owner-rel spec))
  (define path (build-path root rel))
  (define min-cases (suite-owner-min-cases spec))
  (define source (and (file-exists? path) (file->string path)))
  (cond
    [(not source) (hasheq 'rel rel 'witnessed? #f 'detail (format "~a: owner file missing" rel))]
    [else
     (define declared (declared-case-count source))
     (define missing
       (for/list ([tok (in-list (suite-owner-tokens spec))]
                  #:unless (string-contains? source tok))
         tok))
     (cond
       [(pair? missing)
        (hasheq 'rel
                rel
                'witnessed?
                #f
                'detail
                (format "~a: refusal vocabulary missing (~a)" rel (string-join missing " / ")))]
       [(< declared min-cases)
        (hasheq 'rel
                rel
                'witnessed?
                #f
                'detail
                (format "~a: only ~a declared case(s), the row pins ~a" rel declared min-cases))]
       [else
        (hasheq 'rel
                rel
                'witnessed?
                #t
                'detail
                (format "~a: ~a declared case(s) (minimum ~a), refusal vocabulary present"
                        rel
                        declared
                        min-cases))])]))

(define (owners-outcome root owners)
  (for/list ([spec (in-list owners)])
    (owner-outcome root spec)))

(define (owners-detail outcomes)
  (string-join (for/list ([o (in-list outcomes)])
                 (hash-ref o 'detail))
               "; "))

;; The injected half: one mutated copy per owner, written under a scratch root
;; that mirrors each owner's repository-relative path. `mutations` is parallel
;; to `owners`; every mutation must turn its owner's witness red.
(define (mutated-owners-injected root owners mutations)
  (define scratch (scratch-dir "owners"))
  (for ([spec (in-list owners)]
        [mutation (in-list mutations)])
    (define rel (suite-owner-rel spec))
    (write-text! (build-path scratch rel) (mutation (file->string (build-path root rel)))))
  (define outcomes (owners-outcome scratch owners))
  (delete-directory/files scratch #:must-exist? #f)
  outcomes)

;; A refusal string the row is made of, rewritten: the mutation the Python
;; fixtures carried (the typed reason / the route spelling the row refuses by
;; stops being named).
(define (rename-token from to)
  (lambda (source)
    (define replaced (regexp-replace* (regexp (regexp-quote from)) source to))
    ;; A mutation that changed nothing would let the injection pass vacuously.
    (when (equal? replaced source)
      (error 'inject-wave-defect "injected mutation ~a matched nothing" from))
    replaced))

;; A dropped guard: the source loses its LAST declared case (the copy is no
;; longer a runnable module, which is the point — the witness must reject it on
;; the declared-case budget before anything is executed).
(define (drop-last-case source)
  (define lines (string-split source "\n"))
  (define at
    (for/last ([line (in-list lines)]
               [i (in-naturals)]
               #:when (string-prefix? (string-trim line) "(test-case"))
      i))
  (unless at
    (error 'inject-wave-defect "no (test-case form to drop"))
  (string-join (take lines at) "\n"))

(define (f6-injected root)
  ;; The F6 defect itself: the resolver's head filter is spelled
  ;; `head={owner}/{repo}:{branch}`, which matches nothing, and the API layer
  ;; stops raising its typed delivery-action refusal.
  (define outcomes
    (mutated-owners-injected root
                             f6-owners
                             (list (rename-token "exn:fail:delivery-action" "exn:fail:delivery")
                                   (rename-token "head=coinerd:" "head=coinerd/q:"))))
  (values
   0
   (format "wrong {owner}/{repo} head filter + non-typed API refusal injected into owner copies: ~a"
           (owners-detail outcomes))
   (andmap (lambda (o) (not (hash-ref o 'witnessed?))) outcomes)))

(define (f6-clean root)
  (define outcomes (owners-outcome root f6-owners))
  (values 0
          (format "native API/read-ladder owners (correct head filter, typed refusals): ~a"
                  (owners-detail outcomes))
          (andmap (lambda (o) (hash-ref o 'witnessed?)) outcomes)))

(define (f11-injected root)
  ;; The F11 defect: a merge under an under-specified authorization, refused only
  ;; by a weakened (non-typed) reason — and, in the second variant, an approval
  ;; case that silently disappears.
  (define variants
    (list (cons "weakened approval refusal string"
                (list (rename-token "no-operator-authorization" "operator-authorization")))
          (cons "dropped approval case" (list drop-last-case))))
  (define outcomes
    (for/list ([variant (in-list variants)])
      (define results (mutated-owners-injected root f11-owners (cdr variant)))
      (hasheq 'note
              (car variant)
              'rejected-every-owner
              (andmap (lambda (o) (not (hash-ref o 'witnessed?))) results)
              'detail
              (owners-detail results))))
  (values 0
          (string-join (for/list ([o (in-list outcomes)])
                         (format "~a -> ~a: ~a"
                                 (hash-ref o 'note)
                                 (if (hash-ref o 'rejected-every-owner) "rejected" "NOT REJECTED")
                                 (hash-ref o 'detail)))
                       " | ")
          (andmap (lambda (o) (hash-ref o 'rejected-every-owner)) outcomes)))

(define (f11-clean root)
  (define outcomes (owners-outcome root f11-owners))
  (values 0
          (format "native merge-suite approval cases (recorded authorization + APPROVED review): ~a"
                  (owners-detail outcomes))
          (andmap (lambda (o) (hash-ref o 'witnessed?)) outcomes)))

;; F7 — stale recorded head and cross-artifact timing disagreement
;; ---------------------------------------------------------------------------

;; The F7 red fixture value: the exact v1.00.30-w4 rollback-drill shape (a
;; stale recorded head plus prose phase timings that contradict the structured
;; `eager-fallback-ms` numbers). It used to be emitted by the frozen generator
;; artifacts/wave-delivery-integrity/v1.00.31-w5/raw/matrix-gen.py
;; (`--emit-f7-fixture`, i.e. `canon(F7_FIXTURE)` = python
;; json.dumps(indent=1, sort_keys=True, ensure_ascii=True) plus a newline).
;; The rehearsal emits it in Racket through the SAME canonical renderer the
;; matrix itself uses, so the bytes are unchanged and the rehearsal needs no
;; Python interpreter at all (§11.5).
(define f7-fixture
  (hash 'schema
        "prepared-env-rollback-drill@1"
        'milestone
        895
        'wave
        "W4"
        'issue
        9690
        'note
        (string-append "F7 red fixture: reconstructs the v1.00.30-w4 rollback-drill "
                       "internal inconsistency (blocked-wave artifacts never landed); "
                       "prose phase timings 260/245 ms contradict eager-fallback-ms "
                       "247/256 ms")
        'steps
        (list (hash 'step 1
                    'name "eager-boundary-fallback"
                    'observed "fallback resolved in 260 ms on the first attempt"
                    'verdict "pass")
              (hash 'step 2
                    'name "restore-path-relink"
                    'observed "restore relink completed in 245 ms"
                    'verdict "pass"))
        'timing
        (hash 'eager-fallback-ms (list 247 256) 'restore-relink-ms (list 245 245))
        'recorded-head
        "8299409c9cf6330a85e66cbe5a9ba445bf739d27"))

(define (f7-injected root)
  (define scratch (scratch-dir "f7"))
  (define fixture (build-path scratch "artifacts/f7-repro/v9.99.99-w0"))
  (write-text! (build-path fixture "rollback-drill.json") (canonical-json f7-fixture))
  (write-text! (build-path fixture "SHA256SUMS")
               (format "~a  artifacts/f7-repro/v9.99.99-w0/rollback-drill.json\n"
                       (sha256-file (build-path fixture "rollback-drill.json"))))
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
  ;; The clean control must be a run the guard ACCEPTS (exit 0 and its success
  ;; verdict), never merely an output that lacks the injected refusal token.
  (values code output (and (zero? code) (string-contains? output "artifact-provenance ok"))))

;; ---------------------------------------------------------------------------
;; F8 — opaque failure surfacing
;; ---------------------------------------------------------------------------

;; The guard is scripts/gsd-delivery-git.rkt's classify-failure (the Python
;; controller whose classify_failure this was extracted from is deleted). The
;; probe is a tiny racket program that requires the REAL module by absolute
;; path and prints the typed reason for one (stderr, args) shape, so the row
;; measures the shipped classifier rather than a copy of it.
(define (f8-classify root stderr-literal)
  (define program
    (string-append "#lang racket/base\n"
                   "(require (file "
                   (format "~s" (path->string (build-path root "scripts" "gsd-delivery-git.rkt")))
                   "))\n"
                   "(displayln (classify-failure \"git\" 128 "
                   stderr-literal
                   " (list \"git\" \"fetch\" \"origin\" \"refs/heads/campaign/x\")))\n"))
  (define tmp (make-temporary-file "q-w6-f8-~a.rkt"))
  (dynamic-wind (lambda () (write-text! tmp program))
                (lambda () (capture (list (racket-exe) (path->string tmp)) root))
                (lambda ()
                  (when (file-exists? tmp)
                    (delete-file tmp)))))

(define (f8-injected root)
  (define-values (code output)
    (f8-classify root "\"fatal: couldn't find remote ref refs/heads/campaign/x\""))
  (values code
          output
          (decided-verdict? code output "remote-ref-missing:" (list "delivery command failed"))))

(define (f8-clean root)
  (define-values (code output) (f8-classify root "\"some unclassified failure\""))
  (values code
          output
          ;; The clean control must be a classifier run that actually succeeded
          ;; and produced the honest generic form -- not a crash whose traceback
          ;; merely lacks the typed token.
          (and (zero? code)
               (string-contains? output "delivery command failed")
               (not (string-contains? output "remote-ref-missing:")))))

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

;; BUG-0077: the honest contract is two-phase. Verifier approval PARKS the
;; attempt as 'awaiting-delivery; only the seeded attempt-bound delivered
;; receipt + terminal delivered journal + delivered handoff authorize the
;; finalizing call. A bare #:delivery-proof mode never completes.
(define (seed-delivered-proof! dir rec idx merge-sha)
  (define pid (campaign-plan-id rec))
  (define durable (load-campaign-record dir pid))
  (define wave (car (campaign-record-waves durable)))
  (define attempt (campaign-wave-current-attempt wave))
  (define head (make-string 40 #\a))
  (set-campaign-wave-delivery-branch! wave "campaign/test")
  (set-campaign-wave-delivery-head-sha! wave head)
  (persist-campaign! dir durable)
  (record-delivery-receipt! dir
                            pid
                            idx
                            (hasheq 'repo
                                    "/repo"
                                    'branch
                                    "campaign/test"
                                    'head
                                    head
                                    'tree
                                    (make-string 40 #\b)
                                    'origin
                                    "https://github.com/example/q.git"
                                    'verified-at
                                    0
                                    'evidence
                                    "Verify passed"
                                    'attempt-id
                                    (campaign-attempt-id attempt)
                                    'attempt-fence
                                    (campaign-attempt-fence-token attempt)))
  ;; The first approval parked an awaiting-delivery handoff before the fixture
  ;; had branch/head provenance. Re-issue that pending handoff after mirroring
  ;; the receipt identity into the durable wave, then reconcile it to the exact
  ;; delivered merge SHA. This matches the honest delivery fixture: receipt,
  ;; wave branch/head, handoff branch/head and terminal journal must all agree.
  (persist-delivery-handoff! dir pid wave "test delivered fixture precondition")
  (reconcile-delivered-handoff! dir pid idx merge-sha)
  (update-delivery-journal! dir pid idx (hasheq 'stage "delivered"))
  merge-sha)

(define (honest-complete! dir rec)
  (define attempt0 (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt0)
                      #:expected-fence-token (campaign-attempt-fence-token attempt0))
  (define merge-sha (make-string 40 #\c))
  (seed-delivered-proof! dir rec 0 merge-sha)
  (define fresh (load-campaign-record dir (campaign-plan-id rec)))
  (define attempt (campaign-wave-current-attempt (car (campaign-record-waves fresh))))
  (try-complete-wave! dir
                      fresh
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt)
                      #:delivered-merge-sha merge-sha))

(define (f9-outcome root complete?)
  (define dir (scratch-dir "f9"))
  (define-values (rec attempt) (f9-fixture dir))
  (define result
    (if complete?
        (honest-complete! dir rec)
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:delivery-proof 'require-delivered)))
  (define status (completion-result-status result))
  (delete-directory/files dir #:must-exist? #f)
  (values 0 (format "completion-result-status: ~a" status) status))

(define (f9-injected root)
  (define-values (code reason status) (f9-outcome root #f))
  (values code reason (eq? status 'delivery-pending-cannot-complete)))

(define (f9-clean root)
  (define-values (code reason status) (f9-outcome root #t))
  ;; The sanctioned carry-forward path must actually COMPLETE the wave; merely
  ;; "not the delivery-pending refusal" would accept a crash.
  (values code reason (eq? status 'done)))

(define (f10-outcome root rollback?)
  (define dir (scratch-dir "f10"))
  (define-values (rec attempt) (f9-fixture dir))
  ;; F10's defect only exists for a wave that was HONESTLY delivered (DONE +
  ;; completion event) and then rolled back; an undelivered wave emits no
  ;; completion event at all (BUG-0077).
  (honest-complete! dir rec)
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
   (row
    "F6"
    "PR resolution uses the head={owner}/{repo}:{branch} filter, which matches nothing"
    "tests/test-gsd-delivery-api-native.rkt + tests/test-gsd-delivery-read-ladder.rkt (native API route / read-ladder owners)"
    '("tests/test-gsd-delivery-api-native.rkt" "tests/test-gsd-delivery-read-ladder.rkt")
    'suite-declares
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
        "scripts/gsd-delivery-git.rkt classify-failure (typed failure classification)"
        '("scripts/gsd-delivery-git.rkt")
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
        "tests/test-gsd-delivery-merge.rkt (native approval cases)"
        '("tests/test-gsd-delivery-merge.rkt")
        'suite-declares
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
        ;; The native owner suites for the two suite-witnessed rows are inputs
        ;; too: the declared-case budget and the refusal vocabulary are the
        ;; witness, so swapping in a vacuous owner must change a recorded digest
        ;; and force regeneration.
        "tests/test-gsd-delivery-api-native.rkt"
        "tests/test-gsd-delivery-read-ladder.rkt"
        "tests/test-gsd-delivery-merge.rkt"
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

(define (rehearsal-head root _committed)
  ;; The head of the tree that was actually rehearsed: the generation-time tip.
  ;; A committed artifact cannot name its own commit, so the recorded value is
  ;; the tip as of generation; the content binding (rehearsal-inputs-digest) is
  ;; what prevents a later commit from silently inheriting the verdict, and the
  ;; adversarial test asserts that binding against the current files.
  (or (git-head root) "unknown"))

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
         pure-verdict?
         inputs-digest
         rehearsal-inputs
         row-outcomes
         rows
         row-id
         register-path
         reproduction-path)
