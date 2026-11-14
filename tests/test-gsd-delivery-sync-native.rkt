#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-sync.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 420
;; Phase B slice S2: the `sync` action of scripts/gsd-delivery.py is ported to
;; Racket (scripts/gsd-delivery-sync.rkt) on the shared bounded-git seam proven
;; by the S1 preflight port (scripts/gsd-delivery-git.rkt), and the coordinator's
;; `sync` stage now dispatches to it natively — python3 is never spawned.
;;
;; Parity law under test: nonblank expected branch; detached HEAD refuses;
;; only the expected branch or the shared `main` checkout may sync (never
;; switching); a clean tree is mandatory (tracked/staged/scratch dirt refuse);
;; refresh is a NON-forced fetch of refs/heads/main, so a rewritten origin/main
;; refuses before any merge; `merge --ff-only` refuses a diverged checkout;
;; success returns {status synchronized, head, branch}. Every refusal leaves
;; HEAD and the current branch untouched, is a typed exn:fail:delivery-action
;; carrying the EXACT reason, and is attributed to one cause per case.
;;
;; Infrastructure vs repository state (the reason chain must stay
;; attributable): the "detached HEAD; refusing synchronization" reason applies
;; ONLY to a genuine symbolic-ref failure. A PATH shim pins that a spent
;; internal budget stays a typed delivery-timeout/budget-exhausted reason and is
;; never relabelled as detached HEAD. The sandbox exec/cleanup branches
;; (group-cleanup-unconfirmed, process-group-unavailable) cannot be triggered
;; on a healthy host, so they are pinned at the classifier level through the
;; SAME seam instance the sync entry consults (classify-exec-failure +
;; infrastructure-refusal?).
(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "helpers/gsd-delivery-goldens.rkt")

(define-runtime-path native-module "../scripts/gsd-delivery-sync.rkt")
(define racket-exe
  (let ([found (find-executable-path "racket")])
    (if found
        (path->string found)
        "racket")))

;; Start from an empty environment: no user/system Git config, credential
;; helpers, network protocols, proxy, tokens, or ambient executable overrides.
(define (fixture-env root [path "/usr/bin:/bin"])
  (define env (make-environment-variables))
  (for ([p (in-list `(("PATH" . ,path) ("HOME" . ,(path->string root))
                                       ("GIT_CONFIG_NOSYSTEM" . "1")
                                       ("GIT_CONFIG_GLOBAL" . "/dev/null")
                                       ("GIT_TERMINAL_PROMPT" . "0")
                                       ("GIT_ALLOW_PROTOCOL" . "file")
                                       ("GIT_AUTHOR_NAME" . "Test")
                                       ("GIT_AUTHOR_EMAIL" . "test@example.invalid")
                                       ("GIT_COMMITTER_NAME" . "Test")
                                       ("GIT_COMMITTER_EMAIL" . "test@example.invalid")))])
    (environment-variables-set! env (string->bytes/utf-8 (car p)) (string->bytes/utf-8 (cdr p))))
  env)

(define (git dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 20
                    #:process-group? #t))
  (unless (and (equal? 0 (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

(define (put path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; One fixture: a bare origin, a work checkout that publishes commits, and a
;; subject clone whose checkout is the synchronization target.
(define (with-world proc)
  (define root (make-temporary-file "native-sync-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (fixture-env root)])
                    (define origin (build-path root "origin.git"))
                    (define work (build-path root "work"))
                    (define subject (build-path root "subject"))
                    (git root "init" "--bare" (path->string origin))
                    (git root "init" "-b" "main" (path->string work))
                    (put (build-path work "tracked") "initial\n")
                    (git work "add" "tracked")
                    (git work "commit" "-m" "initial")
                    (git work "remote" "add" "origin" (path->string origin))
                    (git work "push" "origin" "main")
                    (git root "clone" "-b" "main" (path->string origin) (path->string subject))
                    (proc root origin work subject)))
                (lambda () (delete-directory/files root))))

;; Every call publishes a DISTINCT commit so a fixture can advance origin/main
;; more than once (a repeated identical commit is a no-op failure).
(define advance-count (make-parameter 0))

(define (advance work)
  (define n (add1 (advance-count)))
  (advance-count n)
  (put (build-path work "tracked")
       (if (= n 1)
           "remote\n"
           (format "remote-~a\n" n)))
  (git work "add" "tracked")
  (git work "commit" "-m" (format "remote ~a" n))
  (git work "push" "origin" "main")
  (git work "rev-parse" "HEAD"))

;; The receipt-bound campaign state the coordinator's sync stage reads. The
;; fixture commit is published so origin/main stays a fast-forward ancestor.
(define (install-campaign! work subject)
  (make-directory* (build-path subject ".planning/waves"))
  (put (build-path subject ".planning/PLAN.md")
       "# Plan: Native sync\n\n## Waves\n\n- [Inbox] W0: Test → waves/W0-test.md\n")
  (put (build-path subject ".planning/waves/W0-test.md") "# Test\n\nGoal: test\n")
  (define record (migrate-campaign! subject))
  (set-campaign-wave-delivery-branch! (car (campaign-record-waves record)) "campaign/w1")
  (persist-campaign! subject record)
  (git subject "add" ".planning")
  (git subject "commit" "-m" "campaign fixture")
  (git subject "push" "origin" "main")
  (git work "pull" "--ff-only" "origin" "main")
  record)

;; PATH is restored on exit so a later fixture never inherits an override.
(define (with-path bin proc)
  (define env (current-environment-variables))
  (define old (environment-variables-ref env #"PATH"))
  (environment-variables-set! env #"PATH" (string->bytes/utf-8 (format "~a:/usr/bin:/bin" bin)))
  (dynamic-wind void
                proc
                (lambda ()
                  (when old
                    (environment-variables-set! env #"PATH" old)))))

;; A python3 spy that fails closed and records any invocation. The native sync
;; stage must never reach it.
(define (with-python-spy root proc)
  (define bin (build-path root "bin"))
  (define marker (build-path root "python-invoked"))
  (make-directory bin)
  (define spy (build-path bin "python3"))
  (put spy (format "#!/bin/sh\nprintf invoked > '~a'\nexit 97\n" marker))
  (file-or-directory-permissions spy #o700)
  (with-path bin (lambda () (proc marker))))

;; ---------- PATH shims -----------------------------------------------------
;; The shared bounded-git runner resolves "git" through the ambient PATH, so a
;; script named git in the FIRST PATH entry IS the executable under test. Each
;; fixture asserts that resolution before trusting any result.
(define (install-shim! root name script)
  (define bin (build-path root "shim-bin"))
  (make-directory* bin)
  (define shim (build-path bin name))
  (put shim script)
  (file-or-directory-permissions shim #o700)
  shim)

;; A shim fixture needs no repository: every git call is intercepted.
(define (with-shim-world script proc)
  (define root (make-temporary-file "native-sync-shim-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (fixture-env root)])
                    (define subject (build-path root "subject"))
                    (make-directory subject)
                    (define shim (install-shim! root "git" script))
                    (with-path (path->string (path-only shim))
                               (lambda ()
                                 ;; The shim IS the executable under test.
                                 (define resolved (find-executable-path "git"))
                                 (check-not-false resolved
                                                  "the fixture must resolve git through PATH")
                                 (check-equal? (and resolved (path->complete-path resolved))
                                               (path->complete-path shim)
                                               "the fixture must resolve git to the shim")
                                 (proc root subject)))))
                (lambda () (delete-directory/files root))))

(define (with-sync-world proc)
  (with-world (lambda (root _origin work subject)
                (with-python-spy root (lambda (marker) (proc work subject marker))))))

(define (native-sync repo branch)
  (dynamic-require native-module 'delivery-sync!)
  ((dynamic-require native-module 'delivery-sync!) repo branch))

;; A refusal is ONLY the shared typed exception, and its message is the typed
;; reason the coordinator surfaces verbatim. Anything else fails closed.
(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

(define (assert-refused-unchanged subject branch expected-message)
  (dynamic-require native-module 'delivery-sync!) ; Missing API is not a refusal.
  (define before (git subject "rev-parse" "HEAD"))
  (define ref (git subject "symbolic-ref" "--short" "HEAD"))
  (check-equal? (delivery-refusal (lambda () (native-sync subject branch))) expected-message)
  (check-equal? (git subject "rev-parse" "HEAD") before)
  (check-equal? (git subject "symbolic-ref" "--short" "HEAD") ref))

(define (run-sync-cli root . args)
  (run-subprocess racket-exe
                  #:args (append (list (path->string (path->complete-path native-module))) args)
                  #:directory root
                  #:environment (current-environment-variables)
                  #:timeout 60
                  #:process-group? #t))

(module+ test
  (test-case "fast-forward expected wave branch and shared main without switching"
    (for ([branch '(wave main)])
      (with-world
       (lambda (_root _origin work subject)
         (when (eq? branch 'wave)
           (git subject "checkout" "-b" "campaign/w1"))
         (define target (advance work))
         (define result (native-sync subject "campaign/w1"))
         (check-equal? (hash-ref result 'status) "synchronized")
         (check-equal? (hash-ref result 'head) target)
         (check-equal? (hash-ref result 'branch) (if (eq? branch 'wave) "campaign/w1" "main"))
         (check-equal? (git subject "rev-parse" "HEAD") target)
         (check-equal? (git subject "symbolic-ref" "--short" "HEAD") (hash-ref result 'branch))))))

  (test-case "an unrelated branch refuses with the exact unrelated-branch reason"
    (with-world
     (lambda (_root _origin work subject)
       (advance work)
       (git subject "checkout" "-b" "unrelated")
       (assert-refused-unchanged
        subject
        "campaign/w1"
        "synchronization refused: HEAD is 'unrelated', expected 'campaign/w1' (unrelated branch changes not accepted)"))))

  (test-case "a nonblank expected branch is mandatory (blank and missing refuse)"
    ;; Attributed on its own: the missing flag is never the unrelated-branch
    ;; reason, and it never touches the repository.
    (for ([branch (list "" #f)])
      (with-world (lambda (_root _origin work subject)
                    (advance work)
                    (assert-refused-unchanged subject branch "sync requires --expected-branch")))))

  (test-case "detached HEAD refuses"
    (with-world (lambda (_root _origin work subject)
                  (advance work)
                  (git subject "checkout" "--detach")
                  (define before (git subject "rev-parse" "HEAD"))
                  (check-equal? (delivery-refusal (lambda () (native-sync subject "campaign/w1")))
                                "detached HEAD; refusing synchronization")
                  (check-equal? (git subject "rev-parse" "HEAD") before))))

  (test-case "tracked, staged and scratch untracked dirt each refuse"
    (for ([kind '(tracked staged scratch)])
      (with-world
       (lambda (_root _origin work subject)
         (advance work)
         (case kind
           [(tracked) (put (build-path subject "tracked") "local\n")]
           [(staged)
            (put (build-path subject "tracked") "staged\n")
            (git subject "add" "tracked")]
           [(scratch) (put (build-path subject ".planning/scratch/probe") "probe\n")])
         (define dirt (git subject "status" "--porcelain" "--untracked-files=all"))
         (assert-refused-unchanged subject "campaign/w1" "dirty checkout; refusing synchronization")
         (check-equal? (git subject "status" "--porcelain" "--untracked-files=all") dirt)))))

  (test-case "diverged HEAD refuses without a merge"
    (with-world
     (lambda (_root _origin work subject)
       (put (build-path subject "local") "local\n")
       (git subject "add" "local")
       (git subject "commit" "-m" "local")
       (advance work)
       (assert-refused-unchanged
        subject
        "campaign/w1"
        "delivery command failed (git, exit 128); inspect authentication, repository refs and required artifacts locally"))))

  (test-case "fetch non-fast-forward refusal cannot merge stale tracking ref"
    (with-world
     (lambda (_root origin work subject)
       (advance work)
       (git subject "fetch" "origin" "refs/heads/main:refs/remotes/origin/main")
       (define stale (git subject "rev-parse" "refs/remotes/origin/main"))
       (git work "reset" "--hard" "HEAD^")
       (put (build-path work "other") "replacement\n")
       (git work "add" "other")
       (git work "commit" "-m" "replacement")
       (git work "push" "--force" (path->string origin) "main:main")
       (assert-refused-unchanged
        subject
        "campaign/w1"
        "non-fast-forward: the remote ref diverged; fetch, reanchor the verified head and retry (git, exit 1)")
       (check-equal? (git subject "rev-parse" "refs/remotes/origin/main") stale))))

  (test-case "CLI parity: typed refusal JSON on stdout, exit 2, no stderr"
    (with-world
     (lambda (root _origin work subject)
       (advance work)
       (git subject "checkout" "--detach")
       (define cli
         (run-sync-cli root "sync" "--repo" (path->string subject) "--expected-branch" "campaign/w1"))
       (check-equal? (subprocess-result-exit-code cli) 2)
       (check-equal? (subprocess-result-stderr cli) "")
       (define payload (string->jsexpr (subprocess-result-stdout cli)))
       (check-equal? (hash-ref payload 'status) "delivery-pending")
       (check-equal? (hash-ref payload 'reason) "detached HEAD; refusing synchronization"))))

  ;; ---------- infrastructure causes (PATH shims) ----------------------------
  ;; The shared runner resolves "git" through the ambient PATH, so the shims
  ;; below ARE the executable under test. They pin that an infrastructure fault
  ;; keeps its own typed cause and is NEVER reported as the repository-state
  ;; reason "detached HEAD".
  (test-case "a sync git that outlives the internal budget keeps its typed cause, not detached HEAD"
    (with-shim-world
     (string-append "#!/bin/sh\n"
                    "printf 'GITHUB_TOKEN=ghp_secret\\n' >&2\n"
                    "printf 'fatal: bad credentials\\n' >&2\n"
                    "sleep 60\n"
                    "exit 1\n")
     (lambda (_root subject)
       (environment-variables-set! (current-environment-variables) #"GSD_DELIVERY_BUDGET_SEC" #"10")
       (define started (current-inexact-milliseconds))
       (define reason (delivery-refusal (lambda () (native-sync subject "campaign/w1"))))
       (define elapsed (- (current-inexact-milliseconds) started))
       (check-true (or (string-contains? reason "delivery-timeout")
                       (string-contains? reason "budget-exhausted"))
                   reason)
       ;; the tiny configured budget is the ENFORCED guard (~10 s, not 200 s)
       (check-true (string-contains? reason "internal budget 10s") reason)
       (check-true (string-contains? reason "git") reason)
       ;; the infrastructure cause is never relabelled as repository state
       (check-false (string-contains? reason "detached HEAD") reason)
       (check-true (infrastructure-refusal? reason) reason)
       ;; no child stream and no credential reaches the reason
       (check-false (string-contains? reason "ghp_secret") reason)
       (check-false (string-contains? reason "bad credentials") reason)
       (check-true (< elapsed 40000.0) (format "elapsed ~a ms" elapsed)))))

  (test-case "sandbox exec/cleanup causes keep their identity through the sync seam"
    ;; These branches are unreachable on a healthy host, so they are pinned at
    ;; the classifier the sync entry consults (the same shared seam instance).
    (define cleanup
      (classify-exec-failure
       "git"
       "Failed to execute: group cleanup unconfirmed (watcher death/group quiescence); cleanup unconfirmed"))
    (define no-group
      (classify-exec-failure "git"
                             "Failed to execute: group cleanup confirmation requires Linux /proc"))
    (check-true (string-contains? cleanup "group-cleanup-unconfirmed") cleanup)
    (check-true (string-contains? no-group "process-group-unavailable") no-group)
    ;; a genuine symbolic-ref failure is NOT an infrastructure refusal, so the
    ;; sync guard still maps it to detached HEAD ...
    (check-false (infrastructure-refusal? "detached HEAD; refusing synchronization"))
    ;; ... while both cleanup causes bypass that translation unchanged
    (check-true (infrastructure-refusal? cleanup) cleanup)
    (check-true (infrastructure-refusal? no-group) no-group)
    (check-false (string-contains? cleanup "detached HEAD"))
    (check-false (string-contains? no-group "detached HEAD")))

  ;; Differential parity against the recorded Python answers on identical
  ;; clones: the native port must produce the SAME JSON and exit code, and the
  ;; SAME typed reason for every refusal shape.
  (test-case "differential parity with the recorded Python answers (payload, exit, reason)"
    (with-world
     (lambda (root origin work subject)
       ;; A second, identical clone so a mutating case never repairs the
       ;; fixture for the recorded answer.
       (define twin (build-path root "twin"))
       (git root "clone" "-b" "main" (path->string origin) (path->string twin))
       (define (run-native repo)
         (run-sync-cli root "sync" "--repo" (path->string repo) "--expected-branch" "campaign/w1"))
       ;; The recorded oracle answer for one parity case: the exit code plus the
       ;; parsed stdout payload, captured from `python3 scripts/gsd-delivery.py
       ;; sync` at its final revision (tests/fixtures/gsd-delivery-goldens.json).
       ;; `fixture-state` names the clone state the case was recorded against,
       ;; so two rows can never share a case text.
       (define (oracle-spec fixture-state)
         (hasheq 'action "sync" 'expected_branch "campaign/w1" 'fixture_state fixture-state))
       (define (oracle fixture-state label tokens)
         (golden-outcome "gsd-delivery-sync" tokens label (oracle-spec fixture-state)))
       (define (compare-reasons! fixture-state label tokens)
         (define recorded (oracle fixture-state label tokens))
         (define native (run-native twin))
         (check-equal? (subprocess-result-exit-code native) (hash-ref recorded 'exit))
         (check-equal? (subprocess-result-exit-code native) 2)
         (check-equal? (hash-ref (string->jsexpr (subprocess-result-stdout native)) 'reason)
                       (hash-ref recorded 'reason)))
       (define (on-both thunk)
         (thunk subject)
         (thunk twin))
       ;; success: identical payload and HEAD
       (define target (advance work))
       ;; The only dynamic value in the sync answers is the fast-forwarded head.
       (define sync-tokens (list (cons "head" target)))
       (define recorded-ok
         (oracle "branch=main, clean, behind origin" "sync-success" sync-tokens))
       (define native-ok (run-native twin))
       (check-equal? (hash-ref recorded-ok 'exit) 0)
       (check-equal? (subprocess-result-exit-code native-ok) 0)
       (check-equal? (string->jsexpr (string-trim (subprocess-result-stdout native-ok)))
                     (hash-ref recorded-ok 'value))
       (check-equal? (hash-ref (string->jsexpr (subprocess-result-stdout native-ok)) 'head) target)
       (check-equal? (git twin "rev-parse" "HEAD") target)
       ;; unrelated branch
       (on-both (lambda (repo) (git repo "checkout" "-b" "unrelated")))
       (advance work)
       (compare-reasons! "branch=unrelated, clean" "sync-unrelated-branch" sync-tokens)
       (on-both (lambda (repo) (git repo "checkout" "main")))
       ;; dirty checkout
       (on-both (lambda (repo) (put (build-path repo "tracked") "dirty\n")))
       (compare-reasons! "branch=main, dirty tracked file" "sync-dirty-checkout" sync-tokens)
       (on-both (lambda (repo) (git repo "checkout" "--" "tracked")))
       ;; detached HEAD
       (on-both (lambda (repo) (git repo "checkout" "--detach")))
       (compare-reasons! "detached HEAD" "sync-detached-head" sync-tokens)
       (on-both (lambda (repo) (git repo "checkout" "main")))
       ;; diverged checkout (merge --ff-only refuses)
       (on-both (lambda (repo)
                  (put (build-path repo "local") "local\n")
                  (git repo "add" "local")
                  (git repo "commit" "-m" "local")))
       (compare-reasons! "branch=main, divergent local commit" "sync-diverged-checkout" sync-tokens))))

  ;; ============================================================
  ;; Production dispatch: the coordinator `sync` stage is native
  ;; ============================================================

  (test-case "coordinator sync stage is native and fast-forwards the clean checkout"
    (with-sync-world (lambda (work subject marker)
                       (define record (install-campaign! work subject))
                       (define target (advance work))
                       (define result
                         (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
                       (check-eq? (delivery-effect-result-kind result) 'ok)
                       (check-equal? (hash-ref (delivery-effect-result-data result) 'stage) "sync")
                       (check-equal? (git subject "rev-parse" "HEAD") target)
                       (check-false (file-exists? marker) "the sync stage spawned python3"))))

  (test-case "coordinator syncs the expected wave branch checkout"
    (with-sync-world (lambda (work subject marker)
                       (define record (install-campaign! work subject))
                       (git subject "checkout" "-b" "campaign/w1")
                       (define target (advance work))
                       (define result
                         (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
                       (check-eq? (delivery-effect-result-kind result) 'ok)
                       (check-equal? (git subject "rev-parse" "HEAD") target)
                       (check-equal? (git subject "symbolic-ref" "--short" "HEAD") "campaign/w1")
                       (check-false (file-exists? marker) "the sync stage spawned python3"))))

  (test-case "coordinator refuses an unrelated branch without switching or spawning python3"
    (with-sync-world
     (lambda (work subject marker)
       (define record (install-campaign! work subject))
       (define target (advance work))
       (git subject "checkout" "-b" "unrelated")
       (define before (git subject "rev-parse" "HEAD"))
       (define result (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "synchronization refused"))
       (check-equal? (git subject "rev-parse" "HEAD") before)
       (check-equal? (git subject "symbolic-ref" "--short" "HEAD") "unrelated")
       (check-not-equal? before target)
       (check-false (file-exists? marker) "the sync stage spawned python3"))))

  (test-case "coordinator refuses a dirty checkout without spawning python3"
    (with-sync-world
     (lambda (work subject marker)
       (define record (install-campaign! work subject))
       (advance work)
       (define before (git subject "rev-parse" "HEAD"))
       (put (build-path subject "tracked") "dirty\n")
       (define result (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "dirty checkout"))
       (check-equal? (git subject "rev-parse" "HEAD") before)
       (check-false (file-exists? marker) "the sync stage spawned python3"))))

  (test-case "coordinator refuses a non-fast-forward checkout without merging"
    (with-sync-world (lambda (work subject marker)
                       (define record (install-campaign! work subject))
                       (advance work)
                       (define before (git subject "rev-parse" "HEAD"))
                       (put (build-path subject "local") "local\n")
                       (git subject "add" "local")
                       (git subject "commit" "-m" "local")
                       (define diverged (git subject "rev-parse" "HEAD"))
                       (define result
                         (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
                       (check-eq? (delivery-effect-result-kind result) 'blocked)
                       (check-equal? (git subject "rev-parse" "HEAD") diverged)
                       (check-equal? (git subject "rev-parse" "HEAD^") before)
                       (check-false (file-exists? marker) "the sync stage spawned python3"))))

  (test-case "coordinator refuses a detached HEAD without spawning python3"
    (with-sync-world
     (lambda (work subject marker)
       (define record (install-campaign! work subject))
       (advance work)
       (git subject "checkout" "--detach")
       (define before (git subject "rev-parse" "HEAD"))
       (define result (default-delivery-controller subject (campaign-plan-id record) 0 "sync"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data result) 'reason)
                                     "detached HEAD"))
       (check-equal? (git subject "rev-parse" "HEAD") before)
       (check-false (file-exists? marker) "the sync stage spawned python3")))))
