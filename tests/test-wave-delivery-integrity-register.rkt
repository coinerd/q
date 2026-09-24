#lang racket/base

;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 900
;; The register harness exercises all 13 guard rows, including the full-tree
;; artifact-provenance lint and the wave-gate CLI, so a full run takes ~6
;; minutes; the honest timeout is the measured one, never a truncated pass.

;; This test deliberately declares NO @covers: it exercises no production module.
;; It freezes the v1.00.31 contract artifacts (failure register, red-fixture
;; reproductions, contract document) and cross-checks them against each other and
;; against the committed raw plan excerpt. Claiming @covers here would create a
;; false impact link in tests/.coverage-manifest.json. (From W1 on it also
;; requires scripts/ci/invocation-contract.rkt — a CI tool — to serve as the
;; oracle for F1's guard, not a production module.)

;; Register harness for the frozen v1.00.31 wave-delivery integrity register.
;;
;; W0 deliverable (task 3): a harness that runs each register row's fixture in
;; `expect-refused` mode and reports per-row status, so W6 can re-run it against
;; injected defects.
;;
;; Status vocabulary (deliberately has no "ok"):
;;   guard-status   'unguarded | 'guarded-pass | 'guarded-fail
;;   fixture-status 'reproduced | 'not-reproduced | 'refused | 'not-refused | 'skipped
;;
;; A row with no registered guard reports `unguarded` and is NEVER reported as
;; passing; while unguarded, the row's fixture is checked for reproduction
;; liveness (the defect must still be observable). W1-W5 register one guard per
;; mode with `register-guard!`; from then on the row is exercised through the
;; guard instead, which is what W6 replays against injected defects.
;;
;; This wave changes no production behaviour: no guard is registered here and no
;; green claim is made about any guard.
;;
;; W1 (register F1) registers the first guard — see the section below the harness
;; — so the "no guard" claim is now the frozen W0 statement it was written as,
;; while the tests assert the current per-wave registry state.

(require racket/port
         racket/file
         rackunit
         racket/hash
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         json
         (file "../util/json/checksum.rkt")
         (file "../scripts/ci/invocation-contract.rkt")
         (file "../tests/helpers/w2-mini-git-repo.rkt")
         (file "../scripts/gsd-evidence-bind.rkt")
         (file "../extensions/gsd/campaign-state.rkt")
         (file "../extensions/gsd/campaign-repository.rkt")
         (file "../extensions/gsd/wave-completion.rkt")
         (file "../extensions/gsd/delivery-journal.rkt")
         (file "../extensions/gsd/delivery-receipt.rkt")
         (file "../extensions/gsd/plan-snapshot.rkt")
         (only-in (file "../scripts/gsd-wave-gate.rkt")
                  validate-wave-evidence
                  wave-evidence-result-reasons))

(define-runtime-path q-root "..")
(define-runtime-path register-path
                     "../artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json")
(define-runtime-path reproduction-path
                     "../artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json")
(define-runtime-path
 raw-register-path
 "../artifacts/wave-delivery-integrity/v1.00.31-w0/raw/plan-failure-mode-register.txt")
(define-runtime-path sums-path "../artifacts/wave-delivery-integrity/v1.00.31-w0/SHA256SUMS")
(define-runtime-path contract-doc-path "../docs/reports/WAVE-DELIVERY-INTEGRITY-CONTRACT-v1.00.31.md")

(define (read-json-file p)
  (with-input-from-file p (lambda () (read-json))))

(define register (read-json-file register-path))
(define reproduction (read-json-file reproduction-path))
(define register-rows (hash-ref register 'rows))
(define reproduction-rows (hash-ref reproduction 'reproductions))

(define (row-ref r k [default #f])
  (hash-ref r k default))

;; the four contract columns of a register row, used to prove verbatim agreement.
(define (row-fields r)
  (list (row-ref r 'mode "")
        (row-ref r 'structural-fix "")
        (row-ref r 'owning-wave "")
        (row-ref r 'refusal "")))

(define (repro-for mode)
  (for/first ([r (in-list reproduction-rows)]
              #:when (equal? (row-ref r 'mode) mode))
    r))

;; ============================================================
;; Guard registry (populated by W1-W5; empty at W0)
;; ============================================================

(define guards (make-hash))

;; register-guard! : string? (-> (or/c 'refused 'not-refused)) -> void?
;; A guard receives the reproduction row and reports whether the injected defect
;; was refused. W6 calls the harness in expect-refused mode to replay them.
(define (register-guard! mode proc)
  (hash-set! guards mode proc))
(provide register-guard!)

;; ============================================================
;; Executable fixture: F1 declared producer invocation
;; ============================================================

(define (run-capture argv)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (with-handlers ([exn:fail? (lambda (e) 127)])
        (apply system*/exit-code argv))))
  (values code (get-output-string out) (get-output-string err)))

;; The exact shape the W4 branch declared in
;; .github/actions/prepare-racket-environment/action.yml:146-149.
(define (f1-declared-invocation-reproduced?)
  (define script (path->string (build-path q-root "scripts/ci/compiled-root.rkt")))
  (define racket-exe (or (find-executable-path "racket") "racket"))
  (define-values (code _out err)
    (run-capture (list racket-exe
                       script
                       "build"
                       "--out"
                       "/tmp/q-w0-repro/compiled-root"
                       "--checkout"
                       (path->string (current-directory))
                       "--trusted-label"
                       "q-trusted-producer")))
  (and (not (zero? code)) (regexp-match? #rx"unknown switch: --out" err)))

;; ============================================================
;; Reproduction-liveness checks for unguarded rows
;; ============================================================

;; Reproduction fixtures F2-F10 are RECORDED inputs, not recomputations: they
;; reference an unmerged W4 branch (ce13d038) and live GitHub API responses that
;; do not exist in the checked-out tree, so re-deriving them in CI is impossible
;; by design. F1 is the exception and is executed live below. W6 replays all of
;; them as injections through the guards W1-W5 register; the raw inputs are kept
;; under artifacts/wave-delivery-integrity/v1.00.31-w0/raw/ for offline replay.
(define (hex64? s)
  (and (string? s) (regexp-match? #px"^[0-9a-f]{64}$" s)))

(define (f2-reproduced? r)
  (define computed (row-ref r 'computed))
  (define authored (row-ref r 'authored))
  (and (hex64? computed) (hex64? authored) (not (equal? computed authored))))

(define (f3-reproduced? r)
  (not (equal? (row-ref r 'recorded-implementation-sha) (row-ref r 'actual-head))))

(define (f4-reproduced? r)
  (define paths (row-ref r 'non-evidence-paths '()))
  (and (pair? paths) (member "README.md" paths)))

(define (f5-reproduced? r)
  (equal? 0 (row-ref r 'matching-refs -1)))

(define (f6-reproduced? r)
  (and (null? (row-ref r 'tool-form-result '())) (pair? (row-ref r 'api-form-result '()))))

(define (f7-reproduced? r)
  (and (pair? (row-ref r 'stale-head-pins '()))
       (for/or ([t (in-list (row-ref r 'timing-disagreement '()))])
         (not (row-ref t 'agree #t)))))

(define (f8-reproduced? r)
  (string-contains? (row-ref r 'surfaced-to-operator "") "git, exit 128"))

(define (f9-reproduced? r)
  (define pre (row-ref r 'observed-pre-correction #f))
  (and (hash? pre)
       (string-contains? (row-ref pre 'plan-index-row "") "[DONE] W4")
       (equal? "Status: DONE" (row-ref pre 'wave-doc-header))
       (equal? "done" (row-ref pre 'campaign-record-wave-4))
       (equal? "delivery-pending" (row-ref pre 'delivery-journal))))

(define (f10-reproduced? r)
  (eq? #t (row-ref (row-ref r 'observed #f) 'ledger-leads-record)))

(define liveness-checks
  (hash "F1"
        (lambda (_r) (f1-declared-invocation-reproduced?))
        "F2"
        f2-reproduced?
        "F3"
        f3-reproduced?
        "F4"
        f4-reproduced?
        "F5"
        f5-reproduced?
        "F6"
        f6-reproduced?
        "F7"
        f7-reproduced?
        "F8"
        f8-reproduced?
        "F9"
        f9-reproduced?
        "F10"
        f10-reproduced?))

;; ============================================================
;; Harness
;; ============================================================

(struct row-result (mode owning-wave guard-status fixture-status) #:transparent)

;; run-register : -> (listof row-result)
;;
;; The harness semantics ARE expect-refused mode, so there is no separate mode
;; switch to get wrong: a row with a registered guard is evaluated through that
;; guard and the guard must report 'refused (any other outcome is a failure); a
;; row with no registered guard reports 'unguarded together with its
;; reproduction-liveness result. W6 therefore replays a wave merely by
;; registering the wave's guards with `register-guard!` and calling this.
(define (run-register)
  (for/list ([row (in-list register-rows)])
    (define id (row-ref row 'id))
    (define repro (repro-for id))
    (define guard (hash-ref guards id #f))
    (cond
      [guard
       (define outcome (guard repro))
       (row-result id
                   (row-ref row 'owning-wave)
                   (if (eq? outcome 'refused) 'guarded-pass 'guarded-fail)
                   outcome)]
      [else
       (define check (hash-ref liveness-checks id #f))
       (define reproduced? (and check (check repro)))
       (row-result id
                   (row-ref row 'owning-wave)
                   'unguarded
                   (if reproduced? 'reproduced 'not-reproduced))])))

(provide run-register
         row-result
         row-result-mode
         row-result-owning-wave
         row-result-guard-status
         row-result-fixture-status
         register-rows
         reproduction-rows
         repro-for)

;; ============================================================
;; W1 guard for F1 (invocation contract)
;; ============================================================

;; The row is guarded by the thing W1 actually shipped rather than by a
;; re-statement of its fixture: the guard extracts the declaration from the
;; action file and asks the CLI itself whether every declared flag exists. It is
;; the same check tests/test-workflow-invocation-contract.rkt runs
;; repository-wide, so the row flips back to guarded-fail if either side drifts —
;; the action line or the script's option set.
;; Both directions must hold, or the guard proves nothing:
;;
;;   (1) the LIVE declaration — the action's own declared flags against the
;;       shipped CLI — must verify. A reverted CLI, or an action line naming a
;;       flag the script no longer accepts, fails this half; and
;;   (2) the SAME declaration with an unknown flag injected must be refused by
;;       the contract. A validator that stopped refusing unknown flags — the
;;       inertness F1 was made of — fails this half.
;;
;; Only when both hold does the guard report 'refused, meaning "the contract
;; refuses this defect". A guard satisfied by the healthy tree alone would
;; invert the register contract: under W6's expect-refused replay the guard must
;; report 'refused *because* it catches the injected defect, so it has to
;; observe the refusal, not merely the absence of the bug.
(define (f1-declaration)
  (define action (build-path q-root ".github" "actions" "prepare-racket-environment" "action.yml"))
  (for/first ([i (in-list (extract-declared-invocations action))]
              #:when (equal? (hash-ref i 'target "") "scripts/ci/compiled-root.rkt"))
    i))

;; Injectable so the guard's failure path is observable instead of merely
;; asserted: a test hands it the declaration F1 was made of — one naming a flag
;; the script does not accept — and watches the guard report 'not-refused.
(define f1-live-declaration-override (make-parameter #f))

(define (f1-live-declaration)
  (or (f1-live-declaration-override) (f1-declaration)))

(define (invocation-status declaration)
  (hash-ref (check-invocation declaration q-root) 'status "error"))

(define (f1-defect-refused?)
  (define live (f1-live-declaration))
  (and (hash? live)
       (equal? "ok" (invocation-status live))
       (let ([injected
              (hash-set live 'flags (cons "--f1-injected-unknown-flag" (hash-ref live 'flags '())))])
         (equal? "unknown-flag" (invocation-status injected)))))

(register-guard! "F1" (lambda (_repro) (if (f1-defect-refused?) 'refused 'not-refused)))

;; ============================================================
;; W2 guards for F2/F3/F4 (evidence identity & digest integrity)
;; ============================================================

;; F2 guard: the content digest is COMPUTED by tooling at the exact head, and a
;; hand-authored / mismatched recorded digest is refused with the typed
;; `digest-mismatch` verdict. Both halves must hold: (1) a record carrying a
;; digest that does not equal the computed excluded-evidence digest at the tip
;; is refused; (2) once the tool writes the computed digest (the only sanctioned
;; authoring path), the same record verifies `digest-ok`. Falsifiable: if the
;; recompute-and-compare were removed, half (1) would report digest-ok and the
;; guard would flip to 'not-refused.
(define (f2-defect-refused?)
  (define repo (make-mini-repo! "f2"))
  (mini-commit-file! repo "src/a.rkt" "#lang racket\n" "c1 source")
  (define base (mini-git! repo "rev-parse" "HEAD"))
  (define record "docs/reports/gsd-wave-evidence/v-w2.rktd")
  (define record-path (build-path repo record))
  ;; hand-authored digest that cannot equal the computed empty (digest-excluded)
  ;; value — the F2 incident shape.
  (mini-commit-file! repo
                     record
                     (format "#hasheq((content-digest . ~s))\n" (make-string 64 #\0))
                     "c2 hand-authored digest")
  (define head (mini-git! repo "rev-parse" "HEAD"))
  (define refused?
    (string-prefix? (verify-records repo base head (list record-path)) "digest-mismatch"))
  ;; control: the tool re-authors the correct digest, then it verifies.
  (define written (bind-records! repo base head (list record-path)))
  (define clean? (string-prefix? (verify-records repo base head (list record-path)) "digest-ok"))
  (and refused? clean? (string? written)))
(register-guard! "F2" (lambda (_repro) (if (f2-defect-refused?) 'refused 'not-refused)))

;; F3 guard: evidence `implementation-sha` must equal the durable receipt head;
;; the gate's own #:receipt-head cross-check emits a `head-binding-mismatch`
;; reason when they differ and stays silent on that axis when they match. The
;; evidence-only artifact carries both a valid implementation-sha and the two
;; SHA lengths the gate requires of a receipt head. Missing review/validation
;; artifacts are separate rejections the guard does not rely on. Falsifiable
;; both directions: remove the head-binding check and mismatched? becomes #f.
(define f3-impl-sha (make-string 40 #\a))
(define f3-other-sha (make-string 40 #\d))
(define f3-digest-sha (make-string 64 #\b))
(define (f3-head-binding-reasons? receipt-head)
  (define root (make-temporary-file "q-w2-f3-~a" 'directory))
  (define ev-path (build-path root "evidence.rktd"))
  (display-to-file
   (format (string-append
            "#hasheq((schema-version . 2) (milestone . 896) (wave . \"W2\") (issue . 9725)"
            " (status . \"ready-for-merge\") (implementation-sha . ~s) (content-digest . ~s))\n")
           f3-impl-sha
           f3-digest-sha)
   ev-path
   #:exists 'replace)
  (define evidence
    (call-with-input-file ev-path
                          (lambda (in)
                            (parameterize ([read-accept-reader #f]
                                           [read-accept-lang #f]
                                           [read-accept-graph #f])
                              (read in)))))
  (define result (validate-wave-evidence evidence #:root root #:receipt-head receipt-head))
  (ormap (lambda (reason) (string-contains? reason "head-binding-mismatch"))
         (wave-evidence-result-reasons result)))
(define (f3-defect-refused?)
  (and (f3-head-binding-reasons? f3-other-sha) (not (f3-head-binding-reasons? f3-impl-sha))))
(register-guard! "F3" (lambda (_repro) (if (f3-defect-refused?) 'refused 'not-refused)))

;; F4 guard: the evidence record commit must be evidence-only; a commit that
;; mixes an evidence path with a non-evidence path is refused with
;; `impure-record-commit` naming the foreign path. Exercised through the
;; shipped tool's record-commit-purity: (1) an evidence-only commit range reports
;; `pure`; (2) a range whose later commit mixes an evidence path with a foreign
;; source path reports `impure-record-commit` naming the foreign path.
;; Falsifiable: remove the evidence-only enforcement and (2) reports pure.
(define (f4-defect-refused?)
  (define repo (make-mini-repo! "f4"))
  (mini-commit-file! repo "src/a.rkt" "#lang racket\n" "c1 source")
  (define base (mini-git! repo "rev-parse" "HEAD"))
  ;; control: evidence-only commit range is pure
  (mini-commit-file! repo
                     "docs/reports/gsd-wave-evidence/v.rktd"
                     "#hasheq((schema-version . 2))\n"
                     "c2 evidence-only")
  (define clean? (equal? (record-commit-purity repo base (mini-git! repo "rev-parse" "HEAD")) "pure"))
  ;; defect: a later commit touches an evidence path AND a foreign source path
  (define evidence-path (build-path repo "docs/reports/gsd-wave-evidence/v.rktd"))
  (display-to-file "#hasheq((schema-version . 2) (x . 1))\n" evidence-path #:exists 'replace)
  (display-to-file "#lang racket\n(define x 1)\n" (build-path repo "src/a.rkt") #:exists 'replace)
  (mini-git! repo "add" "-A" ".")
  (mini-git! repo "commit" "-q" "-m" "c3 MIXED evidence+source")
  (define head-mixed (mini-git! repo "rev-parse" "HEAD"))
  (define verdict (record-commit-purity repo base head-mixed))
  (define refused?
    (and (string-prefix? verdict "impure-record-commit: ") (string-contains? verdict "src/a.rkt")))
  (and clean? refused?))
(register-guard! "F4" (lambda (_repro) (if (f4-defect-refused?) 'refused 'not-refused)))

;; ============================================================
;; W3 guards: F5 (remote backing), F8 (typed failures),
;; F9 (premature completion), F10 (outbox two-way)
;; ============================================================

(define (w3-f5-defect-refused?)
  ;; The defect: a Verify verdict records a verified receipt for a branch
  ;; that was never pushed. Refused when the unpublished branch gets only
  ;; the typed remote-pending marker (no receipt, typed blocker) and
  ;; publication resolves it into a real receipt with the marker cleared.
  (define root (make-temporary-file "w3reg-f5-~a" 'directory))
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
  (define refused?
    (let ()
      (verify-with-delivery-receipt root
                                    plan
                                    2
                                    root
                                    (lambda () 'approved)
                                    #:approved? (lambda (v) (eq? v 'approved))
                                    #:evidence (lambda (_) "log")
                                    #:remote-published (lambda (_r _b _h) #f)
                                    #:snapshot (lambda (_) ident))
      (define unpublished-ok?
        (and (not (load-delivery-journal root plan 2))
             (hash? (load-remote-pending root plan 2))
             (equal? (remote-pending-blocker root plan 2) 'branch-not-published)))
      (verify-with-delivery-receipt root
                                    plan
                                    2
                                    root
                                    (lambda () 'approved)
                                    #:approved? (lambda (v) (eq? v 'approved))
                                    #:evidence (lambda (_) "log")
                                    #:remote-published (lambda (_r _b _h) #t)
                                    #:snapshot (lambda (_) ident))
      (define published-ok?
        (and (hash? (load-delivery-journal root plan 2)) (not (load-remote-pending root plan 2))))
      (and unpublished-ok? published-ok?)))
  (delete-directory/files root #:must-exist? #f)
  refused?)

(define (w3-f8-defect-refused?)
  ;; The defect: every subprocess failure surfaces as the bare
  ;; "delivery command failed (<cmd>, exit 128)". Refused when the
  ;; classified shapes come back typed with class and remedy while the
  ;; unclassified shape honestly keeps the generic form.
  (define quoted (format "~s" "fatal: couldn't find remote ref refs/heads/campaign/x"))
  (define program
    (string-append
     "import importlib.util as u\n"
     "spec = u.spec_from_file_location('g', r'"
     (path->string (build-path q-root "scripts" "gsd-delivery.py"))
     "')\n"
     "m = u.module_from_spec(spec)\nspec.loader.exec_module(m)\n"
     "print(m.classify_failure(['git', 'fetch', 'origin', 'refs/heads/campaign/x'], 'git', 128, "
     quoted
     "))\n"))
  (define tmp (make-temporary-file "w3reg-f8-~a.py"))
  (dynamic-wind (lambda () (display-to-file program tmp #:exists 'truncate))
                (lambda ()
                  (define out
                    (string-trim (with-output-to-string
                                  (lambda ()
                                    (void (system (format "python3 ~a" (path->string tmp))))))))
                  (and (string-prefix? out "remote-ref-missing:")
                       (string-contains? out "refs/heads/campaign/x")
                       (string-contains? out "push the verified head first")))
                (lambda () (delete-file tmp))))

(define (w3-f9-defect-refused?)
  ;; The defect: a wave can be marked done with delivery-pending and NO
  ;; journal witness (v1.00.30 W4). Refused when require-delivered refuses
  ;; with the typed result and zero durable movement.
  (define dir (make-temporary-file "w3reg-f9-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (o)
     (display "# Plan: W3 register\n\n## Waves\n\n- [Inbox] W0: Evidence → waves/W0-e.md\n" o)))
  (call-with-output-file (build-path dir ".planning" "waves" "W0-e.md")
                         (lambda (o) (display "# Wave 0\n\nGoal: e\n\n## Verify\n\nraco test .\n" o)))
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (define result
    (try-complete-wave! dir
                        rec
                        0
                        #:verifier-approve? #t
                        #:expected-attempt-id (campaign-attempt-id attempt)
                        #:expected-fence-token (campaign-attempt-fence-token attempt)
                        #:delivery-proof 'require-delivered))
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (define refused?
    (and (eq? (completion-result-status result) 'delivery-pending-cannot-complete)
         (eq? (campaign-wave-status (car (campaign-record-waves durable))) 'verifying)
         (equal? (count-completion-events dir durable) 0)))
  (delete-directory/files dir #:must-exist? #f)
  refused?)

(define (w3-f10-defect-refused?)
  ;; The defect: a rolled-back done wave keeps a leading completion event.
  ;; Refused when the rollback leaves the outbox empty after reconcile and
  ;; the typed invariant reports 'ok.
  (define dir (make-temporary-file "w3reg-f10-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (o)
     (display "# Plan: W3 register\n\n## Waves\n\n- [Inbox] W0: Evidence → waves/W0-e.md\n" o)))
  (call-with-output-file (build-path dir ".planning" "waves" "W0-e.md")
                         (lambda (o) (display "# Wave 0\n\nGoal: e\n\n## Verify\n\nraco test .\n" o)))
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
  (persist-campaign! dir rec)
  (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
  (try-complete-wave! dir
                      rec
                      0
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt))
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (set-campaign-wave-status! (car (campaign-record-waves durable)) 'pending)
  (persist-campaign! dir durable)
  (define rolled (load-campaign-record dir (campaign-plan-id rec)))
  (define leads? (eq? (completion-outbox-invariant? dir rolled) 'outbox-leads-record))
  (reconcile-completion-outbox! dir rolled)
  (define clean?
    (eq? (completion-outbox-invariant? dir (load-campaign-record dir (campaign-plan-id rec))) 'ok))
  (delete-directory/files dir #:must-exist? #f)
  (and leads? clean?))

(register-guard! "F5" (lambda (_repro) (if (w3-f5-defect-refused?) 'refused 'not-refused)))
(register-guard! "F8" (lambda (_repro) (if (w3-f8-defect-refused?) 'refused 'not-refused)))
(register-guard! "F9" (lambda (_repro) (if (w3-f9-defect-refused?) 'refused 'not-refused)))
(register-guard! "F10" (lambda (_repro) (if (w3-f10-defect-refused?) 'refused 'not-refused)))

;; ============================================================
;; W4 guards for F6 (API route contract) and F11 (approval contract)
;; ============================================================

;; Each row is guarded by the thing W4 actually shipped: the offline python
;; contract suites, which fail on the unfixed tree (red-first run retained at
;; artifacts/wave-delivery-integrity/v1.00.31-w4/raw/red-first-fixtures.txt)
;; and pass only once the resolver route and the amended approval contract
;; are in place.
(define (run-python-suite rel)
  (define python (or (find-executable-path "python3") (find-executable-path "python")))
  (and python
       (let-values ([(code _out _err) (run-capture (list python
                                                         (path->string (build-path q-root rel))))])
         (zero? code))))

(define (w4-f6-defect-refused?)
  ;; The defect: resolver routes built with the wrong head filter silently
  ;; disable PR resolution. Refused when the API-contract fixtures pass.
  (run-python-suite "tests/test-gsd-delivery-api-contract.py"))

(define (w4-f11-defect-refused?)
  ;; The defect: an unsatisfiable second-account approval gate. Refused when
  ;; the approval-contract fixtures pass in both refusal directions.
  (run-python-suite "tests/test-gsd-delivery-approval-contract.py"))

(register-guard! "F6" (lambda (_repro) (if (w4-f6-defect-refused?) 'refused 'not-refused)))
(register-guard! "F11" (lambda (_repro) (if (w4-f11-defect-refused?) 'refused 'not-refused)))

;; ============================================================
;; W5 guard for F7 (artifact provenance and determinism)
;; ============================================================

(define (run-racket-test rel)
  (define racket-exe (find-executable-path "racket"))
  (and racket-exe
       (let-values ([(code _out _err) (run-capture (list racket-exe
                                                         (path->string (build-path q-root rel))))])
         (zero? code))))

(define (w5-f7-defect-refused?)
  ;; The defect: declared artifacts with stale recorded heads, divergent
  ;; SHA256SUMS bindings, non-canonical JSON, and prose-vs-structured timing
  ;; disagreement. Refused when the provenance lint's own contract suite
  ;; passes — it asserts the typed refusal on the F7-shaped fixture and the
  ;; green path on the real tree.
  (run-racket-test "tests/test-artifact-provenance.rkt"))

(register-guard! "F7" (lambda (_repro) (if (w5-f7-defect-refused?) 'refused 'not-refused)))

;; ============================================================
;; W6 guards for F12 (sentinel placeholders) and F13 (stale frozen contract)
;; ============================================================

;; Both rows were registered by the operator-directed plan amendment of
;; 2026-09-20 after W0 froze F1-F11: F12 was shipped by W2 (the strict gate
;; refuses placeholder identity/narrative fields), F13 by W5 (a plan-body
;; amendment without a refreshed snapshot is refused). W6 replays them as
;; injections and registers their guards here so `run-register` covers every
;; register row F1-F13 in expect-refused mode.

;; F12 guard: the strict gate refuses a trio whose identity and narrative
;; fields are the literal sentinels the W0 binding draft carried, refuses a trio
;; whose narrative fields are merely too short to be substantive, and accepts
;; the same trio once those fields carry genuine content. All three halves must
;; hold: a gate that refused everything would fail the control, and a gate
;; satisfied by non-empty strings alone (the W0 defect) would fail the sentinel
;; case. The terse case pins the minimum-content half independently, because a
;; placeholder is reported before the length rule can fire.
(define (w6-f12-fixture mode)
  (define root (make-temporary-file "w6reg-f12-~a" 'directory))
  (define impl-sha (make-string 40 #\a))
  (define digest (make-string 64 #\b))
  (define genuine-scope
    (string-append "W6 rehearsal: the sentinel trio is refused by the strict gate "
                   "with the typed placeholder-evidence refusal, while the same "
                   "trio with genuine narrative content is not."))
  (define genuine-report
    (string-append "The rehearsal injects the W0 binding-draft sentinel shape "
                   "(reviewer, timestamp, scope, report and red-first fields all "
                   "PENDING) and observes the refusal; the control replaces every "
                   "sentinel with substantive content."))
  (define genuine-red-first
    (string-append "the injected sentinel trio was refused before any publication "
                   "with the typed placeholder-evidence reason"))
  (define-values (reviewer timestamp scope report red-command red-failure owner rationale)
    (case mode
      [(sentinel)
       (values "PENDING" "PENDING" "PENDING" "PENDING" "PENDING" "PENDING" "PENDING" "PENDING")]
      [(terse)
       (values "R. Reviewer"
               "2026-09-23T00:00:00Z"
               "too short"
               "too short"
               "racket scripts/ci/inject-wave-defect.rkt --row F12"
               "too short"
               "W6 rehearsal operator"
               "documents the injection instead of closing the row by assertion")]
      [else
       (values "Independent Reviewer"
               "2026-09-23T00:00:00Z"
               genuine-scope
               genuine-report
               "racket scripts/ci/inject-wave-defect.rkt --row F12"
               genuine-red-first
               "W6 rehearsal operator"
               "documents the injection instead of closing the row by assertion")]))
  (define (write! rel text)
    (define path (build-path root rel))
    (make-directory* (path-only path))
    (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))
  (write! "scripts/required-pr-checks.policy" "(\"lint\")\n")
  (write! "docs/reports/gsd-wave-evidence/w6-f12.rktd"
          (format (string-append
                   "#hasheq((schema-version . 2) (milestone . 896) (wave . \"W12\")"
                   " (issue . 9731) (status . \"ready-for-merge\")"
                   " (implementation-sha . ~s) (content-digest . ~s)"
                   " (required-checks . (\"lint\"))"
                   " (review-artifact . \"docs/reports/gsd-wave-reviews/w6-f12.rktd\")"
                   " (validation-artifact . \"docs/reports/gsd-wave-validation/w6-f12.rktd\"))\n")
                  impl-sha
                  digest))
  (write!
   "docs/reports/gsd-wave-reviews/w6-f12.rktd"
   (format (string-append "#hasheq((reviewer . ~s) (verdict . \"APPROVED\") (reviewed-sha . ~s)"
                          " (content-digest . ~s) (timestamp . ~s) (scope . ~s) (report . ~s))\n")
           reviewer
           impl-sha
           digest
           timestamp
           scope
           report))
  (write!
   "docs/reports/gsd-wave-validation/w6-f12.rktd"
   (format (string-append "#hasheq((status . \"current\") (milestone . 896) (wave . \"W12\")"
                          " (issue . 9731) (branch . \"campaign/w6-f12\")"
                          " (implementation-sha . ~s) (content-digest . ~s)"
                          " (planning-sync . \"current\")"
                          " (remaining-items . (#hasheq((classification . \"deferred-noncritical\")"
                          " (OWNER . ~s) (RATIONALE . ~s))))"
                          " (red-first . #hasheq((command . ~s) (failure . ~s)))"
                          " (focused-tests . #hasheq((result . \"passed\")))"
                          " (format-compile . #hasheq((result . \"passed\")))"
                          " (lint . #hasheq((result . \"passed\")))"
                          " (fast . #hasheq((result . \"passed\")))"
                          " (review-artifact . \"docs/reports/gsd-wave-reviews/w6-f12.rktd\"))\n")
           impl-sha
           digest
           owner
           rationale
           red-command
           red-failure))
  (define evidence
    (call-with-input-file (build-path root "docs/reports/gsd-wave-evidence/w6-f12.rktd")
                          (lambda (in)
                            (parameterize ([read-accept-reader #f]
                                           [read-accept-lang #f]
                                           [read-accept-graph #f])
                              (read in)))))
  (define reasons
    (wave-evidence-result-reasons
     (validate-wave-evidence evidence #:root root #:actual-content-digest digest)))
  (delete-directory/files root #:must-exist? #f)
  reasons)

(define (w6-f12-defect-refused?)
  (define sentinel-reasons (w6-f12-fixture 'sentinel))
  (define terse-reasons (w6-f12-fixture 'terse))
  (define genuine-reasons (w6-f12-fixture 'genuine))
  (and (ormap (lambda (r) (string-contains? r "placeholder-evidence")) sentinel-reasons)
       (ormap (lambda (r) (string-contains? r "insufficient-review-content")) terse-reasons)
       (not (ormap (lambda (r) (string-contains? r "placeholder-evidence")) genuine-reasons))
       (not (ormap (lambda (r) (string-contains? r "insufficient-review-content")) genuine-reasons))))

;; F13 guard: an authored plan-body amendment after the snapshot was taken is
;; refused with the typed frozen-contract-stale exception and leaves the frozen
;; snapshot untouched; restoring the authored body re-binds cleanly (the
;; control that keeps the guard from refusing every amendment-shaped call).
(define (w6-f13-defect-refused?)
  (define dir (make-temporary-file "w6reg-f13-~a" 'directory))
  (define plan-text "# Plan\n\n- [Inbox] W0: Alpha -> waves/W0-alpha.md\n")
  (define campaign (make-string 64 #\a))
  (define (write! rel text)
    (define path (build-path dir rel))
    (make-directory* (path-only path))
    (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))
  (write! (build-path ".planning" "PLAN.md") plan-text)
  (write! (build-path ".planning" "waves" "W0-alpha.md") "# Wave 0\n\nalpha body\n")
  (define-values (_path frozen-digest) (seed-and-bind-plan-snapshot! dir campaign))
  (write! (build-path ".planning" "PLAN.md")
          (string-append plan-text "\n## Failure Modes\n\n| F14 | body amendment after freeze |\n"))
  (define refused?
    (with-handlers ([exn:fail:gsd-frozen-contract-stale? (lambda (_e) #t)]
                    [exn:fail? (lambda (_e) #f)])
      (seed-and-bind-plan-snapshot! dir campaign)
      #f))
  (define snapshot-intact?
    (and (equal? (snapshot-manifest-digest (load-snapshot-manifest dir campaign)) frozen-digest)
         (equal? (file->string (build-path (snapshot-dir dir campaign) "PLAN.md")) plan-text)))
  (write! (build-path ".planning" "PLAN.md") plan-text)
  (define clean?
    (with-handlers ([exn:fail? (lambda (_e) #f)])
      (seed-and-bind-plan-snapshot! dir campaign)
      #t))
  (delete-directory/files dir #:must-exist? #f)
  (and refused? snapshot-intact? clean?))

(register-guard! "F12" (lambda (_repro) (if (w6-f12-defect-refused?) 'refused 'not-refused)))
(register-guard! "F13" (lambda (_repro) (if (w6-f13-defect-refused?) 'refused 'not-refused)))

(define original-guard-ids (sort (hash-keys guards) string<?))

;; ============================================================
;; Tests
;; ============================================================

(define results (run-register))

(define (statuses)
  (map row-result-guard-status results))

(test-case "register is the frozen F1-F10 set extended by the amended-contract rows F11-F13"
  (check-equal? (map (lambda (r) (row-ref r 'id)) register-rows)
                '("F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12" "F13"))
  (check-true (eq? #t (hash-ref register 'frozen)))
  (check-equal? (hash-ref register 'row-count) (length register-rows))
  ;; the plan digest is recorded provenance for the (repo-external) plan file;
  ;; the in-repo verbatim copy is raw/plan-failure-mode-register.txt, which the
  ;; next test checks the register against. Assert the digest is well formed.
  (check-true (and (regexp-match? #px"^[0-9a-f]{64}$" (row-ref register 'plan-sha256 "")) #t)
              "plan digest is a recorded SHA-256"))

(test-case "every register row carries its contract fields"
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (check-true (and (string? (row-ref r 'mode "")) (non-empty-string? (row-ref r 'mode "")))
                (format "~a mode" id))
    (check-true (non-empty-string? (row-ref r 'structural-fix "")) (format "~a structural-fix" id))
    (check-true (non-empty-string? (row-ref r 'refusal "")) (format "~a refusal" id))
    (check-true (and (member (row-ref r 'owning-wave) '("W1" "W2" "W3" "W4" "W5")) #t)
                (format "~a owning-wave" id))))

(test-case "register matches the raw plan excerpt verbatim"
  (define raw (file->string raw-register-path))
  (define raw-lines (string-split raw "\n"))
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    ;; a mode may appear in both the incident table and the register table; the
    ;; register line is the one carrying the refusal contract.
    (define candidates
      (for/list ([l (in-list raw-lines)]
                 #:when (string-prefix? l (format "| ~a |" id)))
        l))
    (check-true (and (pair? candidates) #t) (format "~a present in raw plan excerpt" id))
    ;; every contract column must survive verbatim - mode, guard, owning wave
    ;; and refusal - not merely the refusal text.
    (check-true (and (for/or ([l (in-list candidates)])
                       (andmap (lambda (v) (string-contains? l v)) (row-fields r)))
                     #t)
                (format "~a (mode, guard, wave, refusal) verbatim from the plan" id)))
  ;; converse: the excerpt may not invent a mode the register does not freeze.
  (define raw-ids
    (for/list ([l (in-list raw-lines)]
               #:when (regexp-match? #px"^\\| (F[0-9]+) \\|" l))
      (second (regexp-match #px"^\\| (F[0-9]+) \\|" l))))
  (check-equal? (sort (remove-duplicates raw-ids) string<?)
                (sort (map (lambda (r) (row-ref r 'id)) register-rows) string<?)
                "raw plan excerpt contains exactly the frozen modes, none invented")
  ;; and the observed-evidence narrative must survive too, not just the four
  ;; contract columns: whatever the register carries as `observed` / evidence
  ;; gap must be readable verbatim from the excerpt, so neither can drift.
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (define observed (row-ref r 'observed ""))
    (define gap (row-ref r 'evidence-gap ""))
    (when (or (non-empty-string? observed) (non-empty-string? gap))
      (define line
        (for/or ([l (in-list raw-lines)]
                 #:when (string-prefix? l (format "| ~a |" id)))
          (and (or (not (non-empty-string? observed)) (string-contains? l observed))
               (or (not (non-empty-string? gap)) (string-contains? l gap))
               l)))
      (check-true (and line #t) (format "~a observed/evidence-gap verbatim from the plan" id)))))

(test-case "contract document agrees with the register"
  (define doc (file->string contract-doc-path))
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (define line
      (for/first ([l (in-list (string-split doc "\n"))]
                  #:when (string-prefix? l (format "| **~a**" id)))
        l))
    (check-true (and line #t) (format "~a row in contract doc" id))
    (check-true (and line (andmap (lambda (v) (string-contains? line v)) (row-fields r)) #t)
                (format "~a (mode, guard, wave, refusal) in contract doc" id)))
  ;; converse: the contract document may not invent a row the register does not
  ;; freeze (a fabricated F-row in the table would otherwise pass unnoticed).
  (define doc-ids
    (for/list ([l (in-list (string-split doc "\n"))]
               #:when (regexp-match? #px"^\\| \\*\\*(F[0-9]+)\\*\\*" l))
      (second (regexp-match #px"^\\| \\*\\*(F[0-9]+)\\*\\*" l))))
  (check-equal? (sort (remove-duplicates doc-ids) string<?)
                (sort (map (lambda (r) (row-ref r 'id)) register-rows) string<?)
                "contract document has exactly the frozen modes, none invented"))

(test-case "every register row has exactly one reproduction"
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (check-equal? (length (for/list ([x (in-list reproduction-rows)]
                                     #:when (equal? (row-ref x 'mode) id))
                            x))
                  1
                  (format "~a reproductions" id))
    (define repro (repro-for id))
    (check-equal? (row-ref repro 'expect) "refused" (format "~a expect" id))
    (check-true
     (and (member (row-ref repro 'guard-status) '("unguarded" "guarded-pass" "guarded-fail")) #t)
     (format "~a guard-status" id))))

(test-case "harness reports every row and never reports an unguarded row as passing"
  (check-equal? (length results) (length register-rows))
  (for ([res (in-list results)])
    (define id (row-result-mode res))
    ;; every reported status comes from the declared vocabulary, which contains
    ;; no passing value at all; an unguarded row is therefore unrepresentable as
    ;; a pass.
    (check-true (and (member (row-result-guard-status res) '(unguarded guarded-pass guarded-fail)) #t)
                (format "~a guard-status in vocabulary" id))
    (check-true (and (member (row-result-fixture-status res)
                             '(reproduced not-reproduced refused not-refused skipped))
                     #t)
                (format "~a fixture-status in vocabulary" id))
    ;; A row is either guarded (W1 onward) or honestly reported unguarded; the two
    ;; must never disagree with the registry.
    (if (hash-has-key? guards id)
        (check-true (and (memq (row-result-guard-status res) '(guarded-pass guarded-fail)) #t)
                    (format "~a status comes from its guard" id))
        (check-eq? (row-result-guard-status res) 'unguarded (format "~a unguarded" id)))))

(test-case "harness expect-refused mode discriminates (self-test of the W6 mechanism)"
  ;; W0's deliverable is the mechanism W6 replays. Prove it actually
  ;; discriminates by registering a stub guard for one row and observing both
  ;; outcomes, then restore the registry to exactly its previous state — in W2
  ;; that means the real F1/F2/F3/F4 guards, not an empty registry.
  (define saved (hash-ref guards "F1" #f))
  (dynamic-wind void
                (lambda ()
                  (register-guard! "F1" (lambda (_r) 'refused))
                  (define passed
                    (for/first ([x (in-list (run-register))]
                                #:when (equal? (row-result-mode x) "F1"))
                      x))
                  (check-eq? (row-result-guard-status passed) 'guarded-pass)
                  (register-guard! "F1" (lambda (_r) 'not-refused))
                  (define failed
                    (for/first ([x (in-list (run-register))]
                                #:when (equal? (row-result-mode x) "F1"))
                      x))
                  (check-eq? (row-result-guard-status failed) 'guarded-fail))
                (lambda ()
                  (if saved
                      (hash-set! guards "F1" saved)
                      (hash-remove! guards "F1"))))
  (check-equal? (sort (hash-keys guards) string<?) original-guard-ids)
  (unless (member "F1" original-guard-ids)
    (check-eq? (for/first ([x (in-list (run-register))]
                           #:when (equal? (row-result-mode x) "F1"))
                 (row-result-guard-status x))
               'unguarded)))

(test-case "unguarded rows still reproduce their defect on the unfixed tree"
  (for ([res (in-list results)]
        #:when (eq? (row-result-guard-status res) 'unguarded))
    (check-eq? (row-result-fixture-status res)
               'reproduced
               (format "~a fixture reproduces" (row-result-mode res)))))

(test-case "W3 guards F1-F5/F8-F10 guarded-pass; W4 adds F6/F11; W5 adds F7/F13; W2 row F12 guarded"
  ;; The register is a per-wave ledger: a row becomes guarded in the wave that
  ;; fixes it and only then. A wave that marked rows guarded without fixing them
  ;; would show up here as an extra entry; a wave that fixed F2/F3/F4 and forgot
  ;; to register their guards would show up as those rows falling back to
  ;; unguarded — and, with the fixes live, as reproduction-liveness failures just
  ;; above.
  (check-equal? original-guard-ids
                '("F1" "F10" "F11" "F12" "F13" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9"))
  (check-equal? (sort (hash-keys guards) string<?) original-guard-ids)
  (for ([id (in-list '("F1" "F10" "F11" "F12" "F13" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9"))])
    (check-eq? (for/first ([r (in-list results)]
                           #:when (equal? (row-result-mode r) id))
                 (row-result-guard-status r))
               'guarded-pass
               (format "~a guarded-pass" id)))
  ;; Every guard is falsifiable. F1's falsification is canonical: handed the
  ;; declaration it was made of — a flag the script does not accept — it must
  ;; report 'not-refused. A guard that could only ever observe the healthy tree
  ;; would prove nothing under W6's expect-refused replay.
  (check-true (f1-defect-refused?))
  (parameterize ([f1-live-declaration-override
                  (hash-set (f1-declaration)
                            'flags
                            (cons "--f1-injected-unknown-flag"
                                  (hash-ref (f1-declaration) 'flags '())))])
    (check-false (f1-defect-refused?)))
  (check-true (f2-defect-refused?))
  (check-true (f3-defect-refused?))
  (check-true (f4-defect-refused?))
  ;; W3: the newly guarded rows are falsifiable too — handed a refusal
  ;; that did not happen, each guard must report 'not-refused.
  (check-true (w3-f5-defect-refused?))
  (check-true (w3-f8-defect-refused?))
  (check-true (w3-f9-defect-refused?))
  (check-true (w3-f10-defect-refused?))
  ;; W5: F7's guard runs the provenance lint's own contract suite, which
  ;; asserts typed refusal on the F7-shaped fixture — passing the suite is
  ;; the refusal witness.
  (check-true (w5-f7-defect-refused?))
  ;; W6: the plan-amendment rows F12/F13 are guarded by the shipped refusals
  ;; (sentinel placeholders, stale frozen contract) and are falsifiable — each
  ;; guard's control half must fail if the refusal half stopped refusing.
  (check-true (w6-f12-defect-refused?))
  (check-true (w6-f13-defect-refused?))
  (for ([r (in-list results)]
        #:unless (member (row-result-mode r)
                         '("F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12" "F13")))
    (check-eq? (row-result-guard-status r)
               'unguarded
               (format "~a still unguarded" (row-result-mode r)))))

(test-case "SHA256SUMS covers every artifact input and matches its content"
  (define sums
    (for/list ([l (in-list (string-split (file->string sums-path) "\n"))]
               #:when (non-empty-string? (string-trim l)))
      (define m (regexp-match #px"^([0-9a-f]{64})  (.+)$" l))
      (check-true (and m #t) (format "well-formed SHA256SUMS line: ~a" l))
      (cons (second m) (third m))))
  (check-true (>= (length sums) 4))
  (for ([entry (in-list sums)])
    (define path (build-path q-root (cdr entry)))
    (check-true (file-exists? path) (format "~a exists" (cdr entry)))
    (check-equal? (sha256-file path)
                  (car entry)
                  (format "~a digest matches SHA256SUMS" (cdr entry)))))
