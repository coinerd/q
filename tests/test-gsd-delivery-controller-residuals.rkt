#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-git.rkt scripts/gsd-delivery-status.rkt scripts/gsd-delivery-pr.rkt scripts/gsd-delivery-sync.rkt
;; @timeout 120

;; Phase B final deletion, part B: the residuals of the deleted Python
;; delivery-controller suite (tests/test-gsd-delivery-controller.py, 167 cases).
;;
;; This file ports ONLY the cases that had no native owner. Every other case of
;; the deleted suite is already owned, unchanged in intent, by:
;;
;;   preflight (ready/absent/drift/remote-ref)      tests/test-gsd-delivery-preflight.rkt
;;   api routes, checks, protection, validate-merge tests/test-gsd-delivery-api-native.rkt
;;   read ladder (resolve-pr / pr-ci / review)      tests/test-gsd-delivery-read-ladder.rkt
;;   open-pr write seam                             tests/test-gsd-delivery-write.rkt
;;   sync (branch/detached/dirt/ff-only/CLI shape)  tests/test-gsd-delivery-sync-native.rkt
;;   status ladder + governance + CLI arg shapes    tests/test-gsd-delivery-status.rkt
;;   merge / prepare / approval contract            tests/test-gsd-delivery-merge.rkt
;;   binding-* ladder + CLI arg shapes              tests/test-gsd-delivery-binding.rkt
;;   publish-head                                   tests/test-gsd-delivery-publish.rkt
;;   auto-review (prepare/binding-review)           tests/test-gsd-delivery-auto-review-native.rkt
;;   failure classification + exit-2 JSON boundary  tests/test-delivery-failure-classification.rkt
;;   per-process api/checks caching                 tests/test-gsd-delivery-api-native.rkt
;;   repair-grade broadcast journal generation      tests/test-gsd-delivery-repair-tail.rkt
;;   delivery coordinator stage wiring              tests/test-gsd-delivery-coordinator.rkt
;;
;; What was left unowned, and is ported here:
;;
;;   A. the process budget: the default under the runtime caller, exhaustion
;;      BEFORE any call, and the per-call timeout clamp to the remaining budget
;;      (the Python tests patched subprocess.run to READ the clamp; here the
;;      shared bounded runner is driven directly).
;;   B. auth-failure classification across every gh dialect with no credential
;;      material ever echoed.
;;   C. the retired `.planning/scratch/` porcelain exemption: the exemption must
;;      not exist, and a literal " -> " path can never be filtered out of dirt.
;;   D. generation selection from the AUTHORITATIVE OUTER journal: a repo child
;;      (`root/q`) still reads the campaign journal above it, and a malformed
;;      history fails closed instead of guessing a branch.
;;   E. the repair binding path is a NEW immutable publication per generation.
;;   F. status requires an attempt-bound receipt and selects its generation
;;      BEFORE any network read; an absent binding stays a typed pending stop.
;;   G. the publication PR: the merged_at association quirk plus the
;;      authoritative pulls/<n> fallback, and fail-closed ambiguity.
;;   H. the shared CLI arg/JSON shapes (flag parsing + delivery-pending JSON).
;;
;; Fast and hermetic: fake gh/git seams for the pure cases, one local git
;; world (a bare origin plus a checkout under a campaign root) for the two
;; cases that need real refs, and no real network.
(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-sync.rkt"
         (only-in "../scripts/gsd-delivery-status.rkt"
                  delivery-status!
                  campaign-journal-root
                  journal-generation
                  publication-pr
                  binding-branch)
         (only-in "../scripts/gsd-delivery-pr.rkt" binding-path))

(define-runtime-path here ".")

(define plan-id (make-string 64 #\a))
(define wave 0)

;; A typed refusal is the shared exception; its message IS the reason the
;; coordinator surfaces verbatim.
(define (refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (error 'residuals "expected a typed delivery refusal, got a value")))

;; ============================================================
;; Fixture plumbing
;; ============================================================

(define (run/result command args #:dir [dir (current-directory)] #:env [env #f])
  (run-subprocess command
                  #:args args
                  #:directory dir
                  #:environment (or env (current-environment-variables))
                  #:timeout 120
                  #:process-group? #t))

(define (run/ok command args #:dir [dir (current-directory)])
  (define result (run/result command args #:dir dir))
  (unless (zero? (subprocess-result-exit-code result))
    (error 'residuals
           "~a ~a failed (exit ~a): ~a"
           command
           args
           (subprocess-result-exit-code result)
           (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

(define (git dir . args)
  (apply run/ok "git" (list (append (list "-C" (path->string dir)) args))))

;; Path comparison that ignores a trailing separator (`path-only` yields one).
(define (same-dir? a b)
  (equal? (explode-path (path->complete-path a)) (explode-path (path->complete-path b))))

(define (put path text)
  (make-parent-directory* path)
  (display-to-file text path #:exists 'truncate)
  path)

(define (executable-script path text)
  (put path text)
  (file-or-directory-permissions path #o755)
  path)

(define (with-temp-dir f)
  (define dir (make-temporary-file "delivery-residuals-~a" 'directory))
  (dynamic-wind void (lambda () (f dir)) (lambda () (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; A. The process budget (Python: _budget_setting / remaining_budget)
;; ============================================================

;; GSD_DELIVERY_BUDGET_SEC is read per construction. Any parse failure or a
;; value outside [10, runtime-caller-5] falls back to the 200 s default, so a
;; mistyped override can never widen (or silently shrink) the budget.
(define (budget-with value)
  (parameterize ([current-environment-variables
                  (let ([env (environment-variables-copy (current-environment-variables))])
                    (if value
                        (begin
                          (environment-variables-set! env
                                                      #"GSD_DELIVERY_BUDGET_SEC"
                                                      (string->bytes/utf-8 value))
                          env)
                        (begin
                          (environment-variables-set! env #"GSD_DELIVERY_BUDGET_SEC" #f)
                          env)))])
    (budget-seconds)))

(module+ test
  (test-case "A1: the default process budget stays under the runtime caller"
    ;; Python: default + 5 <= RUNTIME_CALLER_BUDGET (240 s). The internal
    ;; budget is the ENFORCED guard; the caller's timeout is only its ceiling.
    (check-equal? default-process-budget 200.0)
    (check-equal? runtime-caller-budget 240.0)
    (check-true (<= (+ default-process-budget 5.0) runtime-caller-budget)
                "the default must leave headroom under the 240 s runtime caller")
    (check-equal? (budget-with #f) default-process-budget "unset falls back to the default")
    ;; out of range in EITHER direction, and every unparseable value
    (for ([value (in-list '("1000" "5" "9.99" "236" "-1" "garbage" "" "1e9"))])
      (check-equal? (budget-with value) default-process-budget value))
    ;; in-range values are honoured (the two documented bounds included)
    (check-equal? (budget-with "120") 120.0)
    (check-equal? (budget-with "10") 10.0)
    (check-equal? (budget-with "235") 235.0)
    (check-equal? (budget-with " 120 ") 120.0 "surrounding whitespace is tolerated"))

  ;; A spent budget is refused BEFORE the command is spawned: the runner never
  ;; becomes a way to run "just one more" call. Proven with a command that does
  ;; not exist — a spawn attempt would classify as command-not-found instead.
  (test-case "A2: an exhausted budget fails closed before any call"
    (for ([budget (in-list '(0.0 0.5 1.0))])
      (define runner (make-bounded-runner budget (current-inexact-monotonic-milliseconds)))
      (define reason (refusal (lambda () (runner "q-command-that-does-not-exist-77" '() 45))))
      (check-true (string-prefix? reason "budget-exhausted:") reason)
      (check-true (string-contains? reason "rerun the action separately") reason)
      ;; the typed cause is an infrastructure fault, never repository state
      (check-true (infrastructure-refusal? reason) reason)
      (check-false (string-contains? reason "command-not-found") reason)))

  ;; Every call is clamped to min(requested, remaining - 0.5), so one action
  ;; can never outlive its budget no matter what timeout a helper asks for.
  (test-case "A3: each call's timeout is clamped to the remaining budget"
    (define dir (make-temporary-file "delivery-clamp-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define slow (executable-script (build-path dir "slow.sh") "#!/bin/sh\nsleep 30\nexit 0\n"))
       ;; 1.25 s of budget left, a helper asking for 90 s: the clamp is
       ;; max(0.5, 1.25 - 0.5) = 0.75 s, so the call is killed in ~1 s and the
       ;; reason reports the CLAMPED window, never the requested one.
       (define runner (make-bounded-runner 1.25 (current-inexact-monotonic-milliseconds)))
       (define started (current-inexact-milliseconds))
       (define reason (refusal (lambda () (runner (path->string slow) '() 90))))
       (define elapsed (- (current-inexact-milliseconds) started))
       (check-true (string-prefix? reason "delivery-timeout:") reason)
       ;; whole-seconds(0.75) = 1 and whole-seconds(1.25) = 1
       (check-true (string-contains? reason "within 1s") reason)
       (check-true (string-contains? reason "internal budget 1s") reason)
       (check-false (string-contains? reason "90") "the requested 90 s never reaches the child")
       (check-true (< elapsed 10000.0)
                   (format "the clamp must bound the wall clock, got ~a ms" elapsed)))
     (lambda () (delete-directory/files dir #:must-exist? #f)))))

;; ============================================================
;; B. Auth failures: every gh dialect, no credential material
;; ============================================================

(module+ test
  ;; Python: `_AUTH_ERROR` matched gh auth / GITHUB_TOKEN / bad credentials /
  ;; authentication required / HTTP 40[13]. The native classifier keeps the
  ;; shape but SURFACES a fixed reason: raw stderr may carry a token.
  (test-case "B1: authentication failures are detected across every gh dialect"
    (define dialects
      (list "gh: To get started with GitHub CLI, please run: gh auth login"
            "gh: GITHUB_TOKEN is not set"
            "gh: authentication required"
            "gh: HTTP 401: Unauthorized"
            "gh: HTTP 403: Forbidden (Bad credentials)"))
    (for ([stderr (in-list dialects)])
      (define reason (classify-failure "gh" 1 stderr '("gh" "api" "repos/x")))
      (check-true (string-prefix? reason "GitHub authentication failed") reason)
      (check-true (string-contains? reason "gh auth status") "the remedy is named")
      (check-false (string-contains? reason "exit 1")
                   "the auth refusal never leaks the raw subprocess shape")))

  (test-case "B2: the auth refusal never echoes credential material"
    (define stderr
      (string-append "gh: HTTP 401: Bad credentials - token abc123secret\n"
                     "GITHUB_TOKEN=ghp_verysecretvalue\n"))
    (define reason (classify-failure "gh" 1 stderr '("gh" "api" "repos/x")))
    (for ([needle (in-list
                   '("abc123secret" "ghp_verysecretvalue" "Bad credentials" "GITHUB_TOKEN="))])
      (check-false (string-contains? reason needle) needle))
    ;; the CLI shape an operator sees carries exactly the same typed reason
    (define payload (string->jsexpr (delivery-pending-json reason)))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-equal? (hash-ref payload 'reason) reason)
    (check-false (string-contains? (string-downcase (jsexpr-string payload)) "token")
                 "no credential-shaped text reaches the operator JSON")))

;; ============================================================
;; C. The retired `.planning/scratch/` porcelain exemption
;; ============================================================

(module+ test
  (test-case "C1: no in-tree scratch porcelain exemption exists"
    ;; Python: assertFalse(hasattr(m, 'scratch_exempt_porcelain')). The native
    ;; sync has NO porcelain filter at all — dirt is decided by one exact-empty
    ;; comparison — so no such binding may ever appear again.
    (check-false (dynamic-require (build-path here "../scripts/gsd-delivery-sync.rkt")
                                  'scratch-exempt-porcelain
                                  (lambda () #f)))
    (check-false
     (dynamic-require (build-path here "../scripts/gsd-delivery-sync.rkt") 'porcelain (lambda () #f)))
    (check-false (regexp-match? #px"(?m:^[ \t]*\\([ \t]*define[ \t]*\\([ \t]*[^)\\s]*scratch)"
                                (file->string (build-path here "../scripts/gsd-delivery-sync.rkt")))
                 "the sync module must define no scratch-aware helper")
    (check-false (regexp-match? #px"(?i:scratch)"
                                (file->string (build-path here "../scripts/gsd-delivery-git.rkt")))
                 "the shared seam carries no scratch notion either"))

  (test-case "C2: in-tree scratch and arrow-shaped dirt can never be filtered out"
    ;; Python: a `.planning/scratch/probe` probe AND a path whose literal name
    ;; contains " -> " (which a porcelain-renaming parser would misread as a
    ;; rename source) both stay in the refusal. The scratch-probe half is owned
    ;; by test-gsd-delivery-sync-native.rkt ("tracked, staged and scratch
    ;; untracked dirt each refuse"); this case pins the arrow-shaped name and
    ;; the absence of any filter. One sh script builds the fixture, so the case
    ;; costs two sync calls rather than a dozen git calls.
    (with-temp-dir (lambda (dir)
                     (define setup (build-path dir "setup.sh"))
                     (put setup
                          (string-append "set -e\n"
                                         "cd \"$1\"\n"
                                         "git init -q -b main repo\n"
                                         "cd repo\n"
                                         "git config user.email test@example.invalid\n"
                                         "git config user.name Test\n"
                                         "printf 'base\\n' > README.md\n"
                                         "git add README.md\n"
                                         "git commit -q -m base\n"
                                         "mkdir -p .planning/scratch\n"
                                         "printf 'probe\\n' > .planning/scratch/probe\n"))
                     (run/ok "sh" (list (path->string setup) (path->string dir)))
                     (define repo (build-path dir "repo"))
                     (check-equal? (refusal (lambda () (delivery-sync! repo "campaign/w1")))
                                   "dirty checkout; refusing synchronization")
                     ;; a file literally named "src/x -> .planning/scratch/y" is ordinary dirt
                     (delete-directory/files (build-path repo ".planning") #:must-exist? #f)
                     (put (build-path repo "src" "x -> .planning/scratch/y") "foreign\n")
                     (check-equal? (refusal (lambda () (delivery-sync! repo "campaign/w1")))
                                   "dirty checkout; refusing synchronization")))))

;; ============================================================
;; D. Generation from the AUTHORITATIVE OUTER journal
;; ============================================================

(define (receipt-json #:head [head (make-string 40 #\c)])
  (hasheq 'repo
          "https://github.com/example/q"
          'branch
          "campaign/w0"
          'origin
          "https://github.com/example/q"
          'evidence
          "full Verify passed"
          'head
          head
          'tree
          (make-string 40 #\d)
          'verified-at
          1
          'attempt-id
          "attempt-5"
          'attempt-fence
          2))

(define (journal-json history)
  (if (equal? history 'absent)
      (hasheq 'schema-version
              1
              'plan-id
              plan-id
              'wave
              wave
              'stage
              "governance"
              'receipt
              (receipt-json))
      (hasheq 'schema-version
              1
              'plan-id
              plan-id
              'wave
              wave
              'stage
              "governance"
              'receipt
              (receipt-json)
              'receipt-history
              (for/list ([i (in-range history)])
                (receipt-json)))))

(define (write-journal! root journal)
  (put (build-path root ".planning" "campaigns" plan-id (format "coordinator-w~a.json" wave))
       (string-append (jstring journal) "\n")))

(define (jstring value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

;; A journal seam that answers with a fixed payload and records the root it was
;; asked about, so the OUTER-journal routing is observable without spawning the
;; trusted reader.
(define (fake-journal-run payload seen)
  (lambda (name args timeout)
    (when (equal? name "racket")
      (set-box! seen (list (list-ref args 2) (list-ref args 3) (list-ref args 4))))
    (jstring payload)))

(module+ test
  (test-case "D1: the generation is the append-only receipt-history length"
    (with-temp-dir (lambda (root)
                     (define seen (box #f))
                     (define (generation payload)
                       (journal-generation (fake-journal-run payload seen) root plan-id wave))
                     ;; a missing journal is JSON `false` -> generation 0 (first publication)
                     (check-equal? (generation #f) 0)
                     ;; a journal without history is generation 0
                     (check-equal? (generation (journal-json 'absent)) 0)
                     ;; a reconciled repair tail publishes under -r<n>
                     (check-equal? (generation (journal-json 1)) 1)
                     (check-equal? (generation (journal-json 2)) 2)
                     ;; a malformed history fails closed instead of guessing a branch
                     (check-equal? (refusal (lambda () (generation (hasheq 'receipt-history "nope"))))
                                   "invalid coordinator journal response")
                     (check-equal? (refusal (lambda () (generation '("nope"))))
                                   "invalid coordinator journal response"))))

  (test-case "D2: a repo child reads the authoritative OUTER campaign journal"
    (with-temp-dir
     (lambda (outer)
       ;; Python: journal_generation(root / 'q', plan, 0) uses the OUTER root's
       ;; journal — a `q` checkout inside a campaign tree never gets its own.
       (make-directory (build-path outer "q"))
       (write-journal! outer (journal-json 1))
       (check-true (same-dir? (campaign-journal-root (build-path outer "q") #f) outer))
       (define seen (box #f))
       (check-equal? (journal-generation (fake-journal-run (journal-json 1) seen)
                                         (campaign-journal-root (build-path outer "q") #f)
                                         plan-id
                                         wave)
                     1)
       (define recorded (unbox seen))
       (check-true (same-dir? (car recorded) outer)
                   "the trusted reader must be handed the OUTER campaign root")
       (check-equal? (cdr recorded) (list plan-id (number->string wave)))
       ;; an independent nested q carrying its own .planning is ambiguous
       (make-directory* (build-path outer "q" ".planning"))
       (check-equal? (refusal (lambda () (campaign-journal-root (build-path outer "q") #f)))
                     "ambiguous campaign roots; explicit --campaign-root required")
       ;; a directory that is not named q is its own root
       (make-directory (build-path outer "checkout"))
       (check-true (same-dir? (campaign-journal-root (build-path outer "checkout") #f)
                              (build-path outer "checkout"))))))

  (test-case "D3: an explicit campaign root must be real and never symlinked"
    (with-temp-dir (lambda (root)
                     (define real (build-path root "real"))
                     (make-directory real)
                     (check-true (same-dir? (campaign-journal-root "/nonexistent" real) real))
                     (define alias (build-path root "alias"))
                     (make-file-or-directory-link real alias)
                     (check-equal? (refusal (lambda () (campaign-journal-root "/nonexistent" alias)))
                                   "symlinked campaign root refused")))))

;; ============================================================
;; E. The repair binding path is a NEW immutable publication
;; ============================================================

(module+ test
  (test-case "E1: each republication generation is its own immutable binding path"
    (check-equal? (binding-path plan-id wave)
                  (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan-id))
    (check-equal? (binding-path plan-id wave 0)
                  (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan-id))
    (check-equal? (binding-path plan-id wave 1)
                  (format "docs/reports/gsd-wave-evidence/~a-w0-r1.rktd" plan-id))
    (check-equal? (binding-path plan-id wave 7)
                  (format "docs/reports/gsd-wave-evidence/~a-w0-r7.rktd" plan-id))
    ;; generations never collide, so a repair can never overwrite the gen-0
    ;; publication it supersedes (governance admits one merged PR per branch)
    (check-equal? (remove-duplicates (for/list ([gen (in-range 0 5)])
                                       (binding-path plan-id wave gen))
                                     string=?)
                  (for/list ([gen (in-range 0 5)])
                    (binding-path plan-id wave gen)))
    ;; and every generation pairs with its own binding branch
    (check-equal? (binding-branch plan-id wave 0) (format "binding/aaaaaaaaaaaa-w0"))
    (check-equal? (binding-branch plan-id wave 2) (format "binding/aaaaaaaaaaaa-w0-r2"))
    ;; an invalid generation fails closed (never a coerced/negative suffix)
    (for ([bad (in-list (list -1 1.0 (string->symbol "1") #t "1"))])
      (check-equal? (refusal (lambda () (binding-path plan-id wave bad)))
                    "invalid binding generation"
                    (format "~s" bad)))
    (for ([bad (in-list (list -1 1.0 (string->symbol "1") (make-string 63 #\a)))])
      (check-exn exn:fail? (lambda () (binding-path bad wave))))))

;; ============================================================
;; F. status: receipt first, generation before network, pending when absent
;; ============================================================

;; The physical binding paths the two status cases assert on. The -r1 file is
;; committed EXECUTABLE, so the trio materialization refuses with the exact
;; path it was handed: the reason itself proves which generation status read.
(define r1-relative (format "docs/reports/gsd-wave-evidence/~a-w0-r1.rktd" plan-id))
(define gen0-relative (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan-id))

(define status-world (box #f))
(define status-world-lock (make-semaphore 1))

;; One shared world (a bare origin plus a checkout under a campaign root):
;;   root/                 campaign root (holds .planning/campaigns/<plan>/)
;;   root/q/               the delivery checkout (origin = local bare repo)
;;   origin.git/main       carries ONLY the -r1 binding artifact, executable
(define (build-status-world!)
  (define root (make-temporary-file "delivery-status-residuals-~a" 'directory))
  (define setup (build-path root "setup.sh"))
  ;; ONE child builds the whole world (a bare origin whose main carries only the
  ;; executable -r1 binding artifact, plus a checkout whose GitHub origin is
  ;; transported to that local bare repo). One subprocess instead of ~17.
  (put setup
       (string-append "set -e\n"
                      "cd \"$1\"\n"
                      "git init -q --bare origin.git\n"
                      "git init -q -b main work\n"
                      "cd work\n"
                      "git config user.email test@example.invalid\n"
                      "git config user.name Test\n"
                      "git remote add origin ../origin.git\n"
                      "printf 'base\\n' > README.md\n"
                      "git add README.md\n"
                      "git commit -q -m base\n"
                      "mkdir -p "
                      (path->string (path-only r1-relative))
                      "\n"
                      "printf '#!/bin/sh\\nexit 0\\n' > "
                      r1-relative
                      "\n"
                      "chmod 755 "
                      r1-relative
                      "\n"
                      "git add "
                      r1-relative
                      "\n"
                      "git commit -q -m publish\n"
                      "git push -q origin main\n"
                      "cd ..\n"
                      "git init -q -b main q\n"
                      "cd q\n"
                      "git config user.email test@example.invalid\n"
                      "git config user.name Test\n"
                      "git remote add origin https://github.com/example/q\n"
                      "git config \"url.$1/origin.git.insteadOf\" https://github.com/example/q\n"
                      "git fetch -q origin\n"))
  (run/ok "sh" (list (path->string setup) (path->string root)))
  (write-journal! root (journal-json 1))
  (hash 'root root 'repo (build-path root "q")))

(define (shared-status-world)
  (call-with-semaphore status-world-lock
                       (lambda ()
                         (or (unbox status-world)
                             (let ([world (build-status-world!)])
                               (set-box! status-world world)
                               world)))))

(module+ test
  ;; Python: status(root, plan, 0, campaign_root=root) with NO attempt-bound
  ;; receipt must refuse on the receipt, not on the repository. Proven by the
  ;; nonexistent --repo: any git/gh read would fail with a different reason.
  (test-case "F1: status requires an attempt-bound Verify receipt before any read"
    (with-temp-dir
     (lambda (root)
       (define journal-path
         (build-path root ".planning" "campaigns" plan-id (format "coordinator-w~a.json" wave)))
       (define (status! journal)
         (if (eq? journal 'none)
             (delete-directory/files (build-path root ".planning") #:must-exist? #f)
             (write-journal! root journal))
         (refusal (lambda ()
                    (delivery-status! (build-path root "no-such-checkout")
                                      plan-id
                                      wave
                                      #:campaign-root root))))
       ;; no journal at all
       (check-equal? (status! 'none) "delivery proof requires the authoritative coordinator journal")
       ;; a journal whose receipt is not attempt-bound is not a Verify receipt
       ;; (it still passes the trusted reader's own schema)
       (for ([broken (in-list (list (hash-remove (receipt-json) 'attempt-id)
                                    (hash-set (receipt-json) 'branch "main")))])
         (check-equal? (status! (hash-set (journal-json 'absent) 'receipt broken))
                       "delivery proof requires an attempt-bound Verify receipt"))
       (check-true (file-exists? journal-path) "the fixture always writes a journal"))))

  (test-case "F2: status selects its generation before the publication read"
    (define world (shared-status-world))
    (define root (hash-ref world 'root))
    (define repo (hash-ref world 'repo))
    ;; generation 0 (no repair history): the gen-0 binding is genuinely absent
    (write-journal! root (journal-json 'absent))
    (check-equal? (refusal (lambda () (delivery-status! repo plan-id wave #:campaign-root root)))
                  (format "W~a verified; delivery pending: protected campaign binding is absent"
                          wave))
    ;; generation 1: the SAME journal now carries one prior receipt, so status
    ;; must read the -r1 publication — the committed executable artifact makes
    ;; the trio gate name that exact path, proving the generation was selected
    ;; before the read rather than defaulting to the gen-0 path.
    (write-journal! root (journal-json 1))
    (define reason (refusal (lambda () (delivery-status! repo plan-id wave #:campaign-root root))))
    (check-true (string-contains? reason r1-relative)
                (format "the -r1 publication must be the one read, got: ~a" reason))
    (check-false (string-contains? reason gen0-relative)
                 "the gen-0 path must never be the selected publication")))

(module+ test
  (test-case "G1: a genuinely merged squash PR is accepted with merged null + merged_at"
    (define seen (box '()))
    (define api
      (fake-api (hash (format "commits/~a/pulls" publication) (list (association-pr 9699))) seen))
    (check-equal? (hash-ref (publication-pr api slug publication) 'number) 9699)
    ;; the association payload alone decided it: no authoritative confirmation
    (check-equal? (unbox seen) (list (format "commits/~a/pulls" publication)))
    ;; a `merged: true` payload needs no quirk handling either
    (define seen2 (box '()))
    (define api2
      (fake-api (hash (format "commits/~a/pulls" publication) (list (association-pr 42 #:merged #t)))
                seen2))
    (check-equal? (hash-ref (publication-pr api2 slug publication) 'number) 42)
    (check-equal? (unbox seen2) (list (format "commits/~a/pulls" publication))))

  (test-case "G2: an inconclusive association payload falls back to pulls/<n>"
    ;; neither `merged: true` nor closed+merged_at: the authoritative endpoint
    ;; has the last word (a bare `merged: true` confirms it)
    (define seen (box '()))
    (define api
      (fake-api (hash (format "commits/~a/pulls" publication)
                      (list (association-pr 7777 #:merged #f #:state "open" #:merged-at #f))
                      "pulls/7777"
                      (hasheq 'merged #t))
                seen))
    (check-equal? (hash-ref (publication-pr api slug publication) 'number) 7777)
    (check-not-false (member "pulls/7777" (unbox seen)))
    ;; the authoritative endpoint may itself answer with closed+merged_at
    (define api2
      (fake-api (hash (format "commits/~a/pulls" publication)
                      (list (association-pr 7777 #:merged #f #:state "open" #:merged-at #f))
                      "pulls/7777"
                      (hasheq 'state
                              "closed"
                              'merged
                              #f
                              'merged_at
                              "2026-09-15T10:00:00Z"
                              'merge_commit_sha
                              publication))
                (box '())))
    (check-equal? (hash-ref (publication-pr api2 slug publication) 'number) 7777)
    ;; the authoritative endpoint says NOT merged -> fail closed
    (define api3
      (fake-api (hash (format "commits/~a/pulls" publication)
                      (list (association-pr 7777 #:merged #f #:state "open" #:merged-at #f))
                      "pulls/7777"
                      (hasheq 'merged #f))
                (box '())))
    (check-equal? (refusal (lambda () (publication-pr api3 slug publication)))
                  "binding publication must be exactly one merged same-repo PR"))

  (test-case "G3: ambiguity, a foreign fork and a wrong merge fail closed"
    (define route (format "commits/~a/pulls" publication))
    (define (refuse-with prs)
      (refusal (lambda () (publication-pr (fake-api (hash route prs) (box '())) slug publication))))
    (check-equal? (refuse-with (list (association-pr 1) (association-pr 2)))
                  "binding publication must be exactly one merged same-repo PR")
    (check-equal? (refuse-with (list (association-pr 3 #:head-repo "someone/fork")))
                  "binding publication must be exactly one merged same-repo PR")
    (check-equal? (refuse-with (list (association-pr 4 #:base-repo "other/q")))
                  "binding publication must be exactly one merged same-repo PR")
    (check-equal? (refuse-with (list (association-pr 5 #:merge (make-string 40 #\f))))
                  "binding publication must be exactly one merged same-repo PR")
    ;; no association at all
    (check-equal? (refusal (lambda () (publication-pr (fake-api (hash) (box '())) slug publication)))
                  "binding publication is not associated with a PR")))

;; ============================================================
;; H. The shared CLI arg/JSON shapes
;; ============================================================

(module+ test
  ;; The per-action flag REQUIREMENTS and their exit-2 delivery-pending JSON are
  ;; owned by the native action suites (tests/test-gsd-delivery-status.rkt "the
  ;; S9 CLI ...", tests/test-gsd-delivery-merge.rkt "the S7 CLI refuses missing
  ;; flags and unsupported actions", tests/test-gsd-delivery-sync-native.rkt
  ;; "CLI parity"). What none of them pins is the SHARED parser's refusal shape,
  ;; which every action's CLI inherits.
  (test-case "H1: the shared flag reader never guesses a value"
    (check-equal? (read-flag-value '("--repo" "/x" "--plan" "p")) (hash "--repo" "/x" "--plan" "p"))
    (check-equal? (read-flag-value '()) (hash))
    ;; a flag without a value is a typed refusal, not an empty value
    (check-equal? (refusal (lambda () (read-flag-value '("--repo")))) "unsupported argument: --repo")
    ;; a positional token where a flag belongs is refused (argparse parity)
    (check-equal? (refusal (lambda () (read-flag-value '("status")))) "unsupported argument: status")
    (check-equal? (refusal (lambda () (read-flag-value '("--repo" "/x" "extra"))))
                  "unsupported argument: extra")
    (check-equal? (refusal (lambda () (read-flag-value '("--wave" "1" "--pr"))))
                  "unsupported argument: --pr"))

  (test-case "H2: the delivery-pending JSON is the one exit-2 shape"
    (define payload (string->jsexpr (delivery-pending-json "typing refusal: something")))
    (check-equal? (sort (map symbol->string (hash-keys payload)) string<?) '("reason" "status"))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-equal? (hash-ref payload 'reason) "typing refusal: something")
    ;; single line: the coordinator's typed-stop parser reads stdout whole
    (check-false (string-contains? (delivery-pending-json "a\nb") "\n"))
    ;; and it never drops or rewrites the typed reason
    (for ([reason (in-list (list "budget-exhausted: the delivery process budget (10s) is spent"
                                 "no-review-artifact: the review artifact is missing or malformed"
                                 "invalid binding generation"))])
      (check-equal? (hash-ref (string->jsexpr (delivery-pending-json reason)) 'reason) reason))))

;; ============================================================
;; G. The publication PR: the merged_at quirk plus the authoritative fallback
;; ============================================================

;; Python publication_pr(): the commit-association endpoint reports
;; `merged: null` for a genuine squash merge, so closed+merged_at is accepted
;; and anything inconclusive is CONFIRMED against pulls/<n> before it is
;; trusted. Ambiguity, a foreign fork and a not-merged answer fail closed.
(define slug "example/q")
(define publication (make-string 40 #\a))

(define (association-pr number
                        #:merged [merged 'null]
                        #:state [state "closed"]
                        #:merged-at [merged-at "2026-09-14T17:34:25Z"]
                        #:merge [merge publication]
                        #:head-repo [head-repo slug]
                        #:base-repo [base-repo slug])
  (hasheq 'number
          number
          'merged
          merged
          'state
          state
          'merged_at
          merged-at
          'merge_commit_sha
          merge
          'head
          (hasheq 'ref "campaign/w0" 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref "main" 'sha (make-string 40 #\b) 'repo (hasheq 'full_name base-repo))))

;; A fake api seam: route -> payload, recording every route it was asked for.
(define (fake-api routes seen)
  (lambda (slug* route)
    (set-box! seen (cons route (unbox seen)))
    (hash-ref routes route #f)))
