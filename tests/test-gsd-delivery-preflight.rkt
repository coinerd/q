#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-preflight.rkt
;; @timeout 420
;; Phase B slice S1: the read-only `preflight` action of scripts/gsd-delivery.py
;; is ported to Racket. These tests pin the Python action's semantics against a
;; real local-bare-origin fixture (GitHub identity via a configured insteadOf
;; transport rewrite — repository identity is never rewritten) and prove the
;; production `delivery-preflight` stage of delivery-coordinator.rkt no longer
;; spawns python3. Ten git-fixture cases cost ~55 s locally (each case clones
;; and fetches), so the per-file timeout stays well above the 120 s default.
;;
;; The failure-cause cases below use PATH shims instead of a repository: the
;; module resolves "git" through the ambient PATH (find-executable-path), so a
;; script named git is the executable under test. They pin the typed reason for
;; each cause (GitHub authentication, missing remote ref, internal-budget
;; timeout, missing executable) and prove no child stream — and specifically no
;; credential — ever reaches the reason or the CLI stdout/stderr. The
;; unreachable-in-CI group-cleanup branches are covered by the pure classifier.
(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-preflight.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt" record-delivery-receipt!)
         "../extensions/gsd/delivery-coordinator.rkt"
         "helpers/gsd-delivery-goldens.rkt")

(define-runtime-path preflight-script "../scripts/gsd-delivery-preflight.rkt")
(define racket-exe
  (let ([found (find-executable-path "racket")])
    (if found
        (path->string found)
        "racket")))
(define racket-bin-dir (path->string (path-only (string->path racket-exe))))

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

(define policy-text "(\"lint\" \"test (0)\" \"test-aggregate\")\n")

;; One real fixture: a bare origin, a work checkout that publishes the wave
;; branch, and a subject clone whose CONFIGURED origin URL is a GitHub
;; identity with a local file:// insteadOf transport rewrite (exactly the
;; distinction repository() makes).
(define (with-world proc)
  (define root (make-temporary-file "delivery-preflight-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (fixture-env root)])
                    (define origin (build-path root "origin.git"))
                    (define work (build-path root "work"))
                    (define subject (build-path root "subject"))
                    (git root "init" "--bare" (path->string origin))
                    (git root "init" "-b" "main" (path->string work))
                    (put (build-path work "scripts/required-pr-checks.policy") policy-text)
                    (put (build-path work "tracked") "initial\n")
                    (git work "add" ".")
                    (git work "commit" "-m" "initial")
                    (git work "remote" "add" "origin" (path->string origin))
                    (git work "push" "origin" "main")
                    (git root "clone" "-b" "main" (path->string origin) (path->string subject))
                    (git subject "config" "remote.origin.url" "https://github.com/example/q.git")
                    (git subject
                         "config"
                         (format "url.file://~a.insteadOf" (path->string origin))
                         "https://github.com/example/q.git")
                    (proc root origin work subject)))
                (lambda () (delete-directory/files root))))

(define (commit! work relative text message)
  (put (build-path work relative) text)
  (git work "add" "-A")
  (git work "commit" "-m" message)
  (git work "rev-parse" "HEAD"))

;; Stay on the wave branch: later commits in a test must extend THAT branch.
(define (start-wave-branch! work)
  (git work "checkout" "-q" "-b" "campaign/w1" "main"))

(define (push-wave-commit! work relative text message)
  (define head (commit! work relative text message))
  (git work "push" "origin" "HEAD:refs/heads/campaign/w1")
  head)

(define (run-cli root . args)
  (run-subprocess racket-exe
                  #:args (append (list (path->string preflight-script)) args)
                  #:directory root
                  #:environment (current-environment-variables)
                  #:timeout 180
                  #:process-group? #t))

;; ---------- PATH shims -----------------------------------------------------
;; The delivery runner resolves "git" through the ambient PATH, so a script
;; named git in the FIRST PATH entry IS the executable under test. Install a
;; shim only after every real-git fixture call has finished.
(define (install-shim! root name script)
  (define bin (build-path root "shim-bin"))
  (make-directory* bin)
  (define shim (build-path bin name))
  (put shim script)
  (file-or-directory-permissions shim #o700)
  bin)

(define (with-path bin proc)
  (define env (current-environment-variables))
  (define old (environment-variables-ref env #"PATH"))
  (environment-variables-set! env #"PATH" (string->bytes/utf-8 (format "~a:/usr/bin:/bin" bin)))
  (dynamic-wind void
                proc
                (lambda ()
                  ;; restore only when the fixture had a PATH (it always does)
                  (when old
                    (environment-variables-set! env #"PATH" old)))))

(define (refusal-message thunk)
  (with-handlers ([exn:fail:delivery-preflight? exn-message])
    (thunk)))

;; A shim environment needs no repository: every git call is intercepted.
(define (with-shim-world script proc)
  (define root (make-temporary-file "delivery-shim-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (fixture-env root)])
                    (define subject (build-path root "subject"))
                    (make-directory subject)
                    (with-path (install-shim! root "git" script) (lambda () (proc root subject)))))
                (lambda () (delete-directory/files root))))

(define (preflight-refusal subject)
  (refusal-message
   (lambda ()
     (delivery-preflight! subject (make-string 64 #\a) 0 (make-string 40 #\b) "campaign/w1"))))

(module+ test
  (test-case "published receipt head at origin is ready and nothing is mutated"
    (with-world (lambda (_root _origin work subject)
                  (start-wave-branch! work)
                  (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
                  (define before (git subject "rev-parse" "HEAD"))
                  (define result
                    (delivery-preflight! subject (make-string 64 #\a) 0 head "campaign/w1"))
                  (check-equal? (hash-ref result 'status) "ready")
                  (check-equal? (hash-ref result 'plan-id) (make-string 64 #\a))
                  (check-equal? (hash-ref result 'wave) 0)
                  (check-equal? (hash-ref result 'branch) "campaign/w1")
                  (check-equal? (hash-ref result 'head) head)
                  (check-equal? (hash-ref result 'receipt-head) head)
                  ;; read-only: no branch switch, no checkout mutation
                  (check-equal? (git subject "rev-parse" "HEAD") before)
                  (check-equal? (git subject "symbolic-ref" "--short" "HEAD") "main")
                  (check-equal? (git subject "status" "--porcelain") ""))))

  (test-case "evidence-only tail after the receipt head is ready; source drift refuses"
    (with-world
     (lambda (_root _origin work subject)
       (start-wave-branch! work)
       (define receipt (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       (define tip
         (push-wave-commit! work "docs/reports/gsd-wave-evidence/a-w1.rktd" "()\n" "evidence tail"))
       (define ready (delivery-preflight! subject (make-string 64 #\b) 0 receipt "campaign/w1"))
       ;; the published tip is read back, the receipt identity is preserved
       (check-equal? (hash-ref ready 'head) tip)
       (check-equal? (hash-ref ready 'receipt-head) receipt)
       ;; a later non-evidence commit on the same branch is refused
       (push-wave-commit! work "src/drift.rkt" "# drift\n" "source drift")
       (define refusal
         (with-handlers ([exn:fail:delivery-preflight? exn-message])
           (delivery-preflight! subject (make-string 64 #\b) 0 receipt "campaign/w1")))
       (check-true (string? refusal))
       (check-true (string-contains? refusal "non-evidence paths") refusal)
       (check-true (string-contains? refusal "re-verify before preflight") refusal)
       (check-true (string-contains? refusal "src/drift.rkt") refusal))))

  (test-case "published tip that does not descend from the receipt head refuses"
    (with-world
     (lambda (_root _origin work subject)
       ;; a receipt identity from an unrelated branch, fetched so the local
       ;; object database can prove (or disprove) ancestry
       (git work "checkout" "-q" "-b" "other" "main")
       (define receipt (commit! work "side" "side\n" "side commit"))
       (git work "push" "origin" "other:other")
       (git work "checkout" "-q" "main")
       (git subject "fetch" "--no-tags" "origin" "refs/heads/other:refs/remotes/origin/other")
       (start-wave-branch! work)
       (push-wave-commit! work "tracked" "wave\n" "wave commit")
       (define refusal
         (with-handlers ([exn:fail:delivery-preflight? exn-message])
           (delivery-preflight! subject (make-string 64 #\c) 0 receipt "campaign/w1")))
       (check-true (string? refusal))
       (check-true (string-contains? refusal "is not an ancestor of tip") refusal)
       (check-true (string-contains? refusal "may have been rewritten") refusal)
       (check-true (string-contains? refusal (substring receipt 0 12)) refusal))))

  (test-case "unpublished branch refuses with the typed branch-not-published remedy"
    (with-world (lambda (_root _origin work subject)
                  (start-wave-branch! work)
                  (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
                  ;; never published: the exact ref the receipt names does not exist
                  (define refusal
                    (with-handlers ([exn:fail:delivery-preflight? exn-message])
                      (delivery-preflight! subject (make-string 64 #\d) 0 head "campaign/absent")))
                  (check-true (string? refusal))
                  (check-true (string-contains? refusal "branch-not-published") refusal)
                  (check-true (string-contains? refusal "campaign/absent") refusal)
                  (check-true (string-contains? refusal (substring head 0 12)) refusal)
                  (check-true (string-contains? refusal "push the verified head") refusal)
                  (check-true (string-contains? refusal "remote-ref-missing") refusal))))

  (test-case "missing and malformed required-check policy at origin/main refuse"
    (with-world
     (lambda (_root _origin work subject)
       (start-wave-branch! work)
       (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       (git work "checkout" "-q" "main")
       (git work "rm" "scripts/required-pr-checks.policy")
       (git work "commit" "-m" "drop policy")
       (git work "push" "origin" "main")
       (define main (git work "rev-parse" "HEAD"))
       (define absent
         (with-handlers ([exn:fail:delivery-preflight? exn-message])
           (delivery-preflight! subject (make-string 64 #\e) 0 head "campaign/w1")))
       (check-true (string-contains? absent (format "required-check policy is absent at ~a" main))
                   absent)
       ;; a duplicate-name policy is malformed, not silently accepted
       (put (build-path work "scripts/required-pr-checks.policy") "(\"lint\" \"lint\")\n")
       (git work "add" "-A")
       (git work "commit" "-m" "duplicate policy")
       (git work "push" "origin" "main")
       (define malformed
         (with-handlers ([exn:fail:delivery-preflight? exn-message])
           (delivery-preflight! subject (make-string 64 #\e) 0 head "campaign/w1")))
       (check-true (string-contains? malformed "malformed required-check policy at") malformed))))

  (test-case "dirt and unrelated local work in the checkout are irrelevant (no cleanliness gate)"
    (with-world (lambda (_root _origin work subject)
                  (start-wave-branch! work)
                  (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
                  (put (build-path subject "tracked") "locally modified\n")
                  (put (build-path subject "scratch") "untracked\n")
                  (define result
                    (delivery-preflight! subject (make-string 64 #\f) 0 head "campaign/w1"))
                  (check-equal? (hash-ref result 'status) "ready")
                  (check-equal? (hash-ref result 'head) head))))

  (test-case "invalid campaign/wave identity and missing flags refuse with the Python reasons"
    (with-world
     (lambda (_root _origin work subject)
       (start-wave-branch! work)
       (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       (check-equal? (with-handlers ([exn:fail:delivery-preflight? exn-message])
                       (delivery-preflight! subject "not-a-plan" 0 head "campaign/w1"))
                     "invalid campaign id")
       (check-equal? (with-handlers ([exn:fail:delivery-preflight? exn-message])
                       (delivery-preflight! subject (make-string 64 #\a) -1 head "campaign/w1"))
                     "invalid wave index")
       (check-equal? (with-handlers ([exn:fail:delivery-preflight? exn-message])
                       (delivery-preflight! subject (make-string 64 #\a) 0 head ""))
                     "preflight requires --expected-branch")
       (check-equal?
        (with-handlers ([exn:fail:delivery-preflight? exn-message])
          (delivery-preflight! subject (make-string 64 #\a) 0 (substring head 0 12) "campaign/w1"))
        "preflight requires --expected-head (the durable receipt head)")
       ;; the budget setting never weakens the check: an out-of-range
       ;; GSD_DELIVERY_BUDGET_SEC falls back to the 200 s default
       (environment-variables-set! (current-environment-variables) #"GSD_DELIVERY_BUDGET_SEC" #"0")
       (define ready (delivery-preflight! subject (make-string 64 #\a) 0 head "campaign/w1"))
       (check-equal? (hash-ref ready 'status) "ready"))))

  (test-case "CLI parity: JSON ready on stdout with exit 0, JSON typed refusal with exit 2"
    (with-world
     (lambda (root _origin work subject)
       (start-wave-branch! work)
       (define plan (make-string 64 #\1))
       (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       (define ready
         (run-cli root
                  "preflight"
                  "--repo"
                  (path->string subject)
                  "--plan"
                  plan
                  "--wave"
                  "0"
                  "--expected-head"
                  head
                  "--expected-branch"
                  "campaign/w1"))
       (check-equal? (subprocess-result-exit-code ready) 0 (subprocess-result-stderr ready))
       (define ready-json (string->jsexpr (subprocess-result-stdout ready)))
       (check-equal? (hash-ref ready-json 'status) "ready")
       (check-equal? (hash-ref ready-json 'head) head)
       (check-equal? (hash-ref ready-json 'receipt-head) head)
       (check-equal? (hash-ref ready-json 'branch) "campaign/w1")
       (check-equal? (hash-ref ready-json 'plan-id) plan)
       (check-equal? (hash-ref ready-json 'wave) 0)
       (check-false (string-contains? (subprocess-result-stderr ready) "fatal:"))
       (define refused
         (run-cli root
                  "preflight"
                  "--repo"
                  (path->string subject)
                  "--plan"
                  plan
                  "--wave"
                  "0"
                  "--expected-head"
                  head
                  "--expected-branch"
                  "campaign/absent"))
       (check-equal? (subprocess-result-exit-code refused) 2)
       (check-equal? (subprocess-result-stderr refused) "")
       (define refused-json (string->jsexpr (subprocess-result-stdout refused)))
       (check-equal? (hash-ref refused-json 'status) "delivery-pending")
       (check-true (string-contains? (hash-ref refused-json 'reason) "branch-not-published")))))

  (test-case "a failing git that echoes a token yields the typed auth reason and leaks nothing"
    (with-shim-world (string-append "#!/bin/sh\n"
                                    "printf 'GITHUB_TOKEN=ghp_secret\\n' >&2\n"
                                    "printf 'fatal: bad credentials\\n' >&2\n"
                                    "printf 'shim-stdout-marker\\n'\n"
                                    "exit 1\n")
                     (lambda (root subject)
                       (define reason (preflight-refusal subject))
                       (check-true (string-contains? reason "GitHub authentication failed") reason)
                       ;; the credential and the raw shapes never leave the classifier
                       (check-false (string-contains? reason "ghp_secret") reason)
                       (check-false (string-contains? reason "bad credentials") reason)
                       (check-false (string-contains? reason "shim-stdout-marker") reason)
                       ;; the CLI streams carry the same typed reason and no secret
                       (define cli
                         (run-cli root
                                  "preflight"
                                  "--repo"
                                  (path->string subject)
                                  "--plan"
                                  (make-string 64 #\a)
                                  "--wave"
                                  "0"
                                  "--expected-head"
                                  (make-string 40 #\b)
                                  "--expected-branch"
                                  "campaign/w1"))
                       (define stdout (subprocess-result-stdout cli))
                       (define stderr (subprocess-result-stderr cli))
                       (check-equal? (subprocess-result-exit-code cli) 2)
                       (check-equal? stderr "" "the shim's stderr must not reach the CLI's stderr")
                       (check-false (string-contains? stdout "ghp_secret") stdout)
                       (check-false (string-contains? stdout "shim-stdout-marker") stdout)
                       (define payload (string->jsexpr stdout))
                       (check-equal? (hash-ref payload 'status) "delivery-pending")
                       (check-equal? (hash-ref payload 'reason) reason))))

  (test-case "a git that reports a missing remote ref yields the typed remote-ref-missing reason"
    (with-shim-world
     (string-append "#!/bin/sh\n"
                    "case \" $* \" in\n"
                    "  *\" config \"*) printf 'https://github.com/example/q.git\\n'; exit 0 ;;\n"
                    "esac\n"
                    "printf \"fatal: couldn't find remote ref refs/heads/campaign/w1\\n\" >&2\n"
                    "exit 128\n")
     (lambda (_root subject)
       (define reason (preflight-refusal subject))
       (check-true (string-contains? reason "remote-ref-missing") reason)
       (check-true (string-contains? reason "refs/heads/campaign/w1") reason)
       (check-true (string-contains? reason "push the verified head first") reason)
       (check-true (string-contains? reason "(git, exit 128)") reason)
       ;; the raw remote text is not the reason
       (check-false (string-contains? reason "couldn't find remote ref") reason))))

  (test-case "a git that outlives the internal budget yields a typed timeout, never a hang"
    (with-shim-world
     (string-append "#!/bin/sh\n" "printf 'GITHUB_TOKEN=ghp_secret\\n' >&2\n" "sleep 60\n" "exit 1\n")
     (lambda (_root subject)
       (environment-variables-set! (current-environment-variables) #"GSD_DELIVERY_BUDGET_SEC" #"10")
       (define started (current-inexact-milliseconds))
       (define reason (preflight-refusal subject))
       (define elapsed (- (current-inexact-milliseconds) started))
       (check-true (or (string-contains? reason "delivery-timeout")
                       (string-contains? reason "budget-exhausted"))
                   reason)
       (check-true (string-contains? reason "git") reason)
       ;; the tiny configured budget is the enforced guard (~10 s, not 200 s)
       (check-true (string-contains? reason "internal budget 10s") reason)
       (check-false (string-contains? reason "ghp_secret") reason)
       (check-true (< elapsed 40000.0) (format "elapsed ~a ms" elapsed)))))

  (test-case "a non-GitHub origin is refused by repository identity"
    (with-world (lambda (_root _origin work subject)
                  (start-wave-branch! work)
                  (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
                  (git subject "config" "remote.origin.url" "https://gitlab.com/example/q.git")
                  (check-equal? (preflight-refusal subject)
                                "origin must identify a GitHub repository (no arbitrary API host)"))))

  (test-case "a missing git executable is a typed command-not-found refusal"
    (define root (make-temporary-file "delivery-nogit-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define empty-bin (build-path root "no-git-here"))
       (make-directory empty-bin)
       (parameterize ([current-environment-variables (fixture-env root (path->string empty-bin))])
         (check-false (find-executable-path "git") "the fixture must not resolve a real git")
         (define subject (build-path root "subject"))
         (make-directory subject)
         (define reason (preflight-refusal subject))
         (check-true (string-contains? reason "command-not-found") reason)
         (check-true (string-contains? reason "git") reason)
         (check-false (string-contains? reason "delivery command failed") reason)))
     (lambda () (delete-directory/files root))))

  (test-case "exec/cleanup failures keep their cause and never echo the diagnostic"
    (check-true (string-contains? (classify-exec-failure "git"
                                                         "Failed to execute: command not found: git")
                                  "command-not-found"))
    (check-true
     (string-contains?
      (classify-exec-failure
       "git"
       "Failed to execute: group cleanup unconfirmed (watcher death/group quiescence); cleanup unconfirmed")
      "group-cleanup-unconfirmed"))
    (check-true
     (string-contains?
      (classify-exec-failure "git"
                             "Failed to execute: group cleanup confirmation requires Linux /proc")
      "process-group-unavailable"))
    (check-true (string-contains? (classify-exec-failure "git" "") "command-unavailable"))
    ;; raw diagnostics (here: a URL with embedded credentials) never pass
    (check-false (string-contains? (classify-exec-failure
                                    "git"
                                    "Failed to execute: fatal: https://u:ghp_secret@github.com/x")
                                   "ghp_secret")))

  (test-case "differential parity with the recorded Python answers (same JSON, same exit, same reason)"
    (with-world
     (lambda (root _origin work subject)
       (define plan (make-string 64 #\2))
       (start-wave-branch! work)
       (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       ;; The recorded answers were captured from the Python action, which shells
       ;; the trusted datum reader, so keep the Racket binary reachable for the
       ;; native run too.
       (environment-variables-set! (current-environment-variables)
                                   #"PATH"
                                   (string->bytes/utf-8 (format "~a:/usr/bin:/bin"
                                                                racket-bin-dir)))
       ;; The only dynamic value in the recorded answers is the wave head.
       (define parity-tokens (list (cons "head" head)))
       ;; The recorded oracle answer for one parity case: the exit code plus the
       ;; parsed stdout payload, captured from `python3 scripts/gsd-delivery.py
       ;; preflight` at its final revision
       ;; (tests/fixtures/gsd-delivery-goldens.json). `fixture-state` names the
       ;; repository state the case was recorded against, so two rows can never
       ;; share a case text.
       (define (oracle-spec fixture-state branch)
         (hasheq 'action
                 "preflight"
                 'plan
                 plan
                 'wave
                 0
                 'expected_head
                 head
                 'expected_branch
                 branch
                 'fixture_state
                 fixture-state))
       (define (oracle fixture-state label branch)
         (golden-outcome "gsd-delivery-preflight"
                         parity-tokens
                         label
                         (oracle-spec fixture-state branch)))
       (define recorded-ready (oracle "wave branch published at the expected head"
                                      "preflight-ready"
                                      "campaign/w1"))
       (define native-ready
         (run-cli root
                  "preflight"
                  "--repo"
                  (path->string subject)
                  "--plan"
                  plan
                  "--wave"
                  "0"
                  "--expected-head"
                  head
                  "--expected-branch"
                  "campaign/w1"))
       (check-equal? (hash-ref recorded-ready 'exit) 0)
       (check-equal? (subprocess-result-exit-code native-ready) 0)
       (check-equal? (string->jsexpr (subprocess-result-stdout native-ready))
                     (hash-ref recorded-ready 'value))
       (define native-refused
         (run-cli root
                  "preflight"
                  "--repo"
                  (path->string subject)
                  "--plan"
                  plan
                  "--wave"
                  "0"
                  "--expected-head"
                  head
                  "--expected-branch"
                  "campaign/absent"))
       (define recorded-refused
         (oracle "wave branch absent on origin" "preflight-absent-branch" "campaign/absent"))
       (check-equal? (hash-ref recorded-refused 'exit) 2)
       (check-equal? (subprocess-result-exit-code native-refused) 2)
       (define native-json (string->jsexpr (subprocess-result-stdout native-refused)))
       (check-equal? (hash-ref native-json 'status)
                     (hash-ref (hash-ref recorded-refused 'value) 'status))
       (check-equal? (hash-ref native-json 'reason) (hash-ref recorded-refused 'reason))
       (check-false (string-contains? (subprocess-result-stderr native-refused) "fatal:")))))

  (test-case "production delivery-preflight stage is native: python3 is never spawned"
    (with-world
     (lambda (root _origin work subject)
       ;; The receipt-bound campaign state the coordinator reads.
       (start-wave-branch! work)
       (define head (push-wave-commit! work "tracked" "wave\n" "wave commit"))
       (make-directory* (build-path subject ".planning/waves"))
       (put (build-path subject ".planning/PLAN.md")
            "# Plan: Native preflight wiring\n\n## Waves\n\n- [Inbox] W0: Test → waves/W0-test.md\n")
       (put (build-path subject ".planning/waves/W0-test.md") "# Test\n\nGoal: test\n")
       (define record (migrate-campaign! subject))
       (set-campaign-wave-delivery-branch! (car (campaign-record-waves record)) "campaign/w1")
       (persist-campaign! subject record)
       (define plan (campaign-plan-id record))
       (record-delivery-receipt! subject
                                 plan
                                 0
                                 (hasheq 'repo
                                         "git@github.com:example/q.git"
                                         'branch
                                         "campaign/w1"
                                         'origin
                                         "https://github.com/example/q.git"
                                         'evidence
                                         (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan)
                                         'head
                                         head
                                         'tree
                                         (make-string 40 #\b)
                                         'attempt-id
                                         "attempt-1"
                                         'attempt-fence
                                         7
                                         'verified-at
                                         1789500000))
       ;; A python3 spy that fails closed and records any invocation.
       (define bin (build-path root "bin"))
       (define marker (build-path root "python-invoked"))
       (make-directory bin)
       (define spy (build-path bin "python3"))
       (put spy (format "#!/bin/sh\nprintf invoked > '~a'\nexit 97\n" marker))
       (file-or-directory-permissions spy #o700)
       (environment-variables-set! (current-environment-variables)
                                   #"PATH"
                                   (string->bytes/utf-8 (format "~a:/usr/bin:/bin"
                                                                (path->string bin))))
       (define ready (default-delivery-controller subject plan 0 "delivery-preflight"))
       (check-eq? (delivery-effect-result-kind ready) 'ok)
       (check-equal? (hash-ref (delivery-effect-result-data ready) 'status) "ready")
       (check-equal? (hash-ref (delivery-effect-result-data ready) 'head) head)
       (check-false (file-exists? marker) "the preflight stage must not spawn python3")
       ;; The same stage still refuses natively: source drift blocks with the
       ;; typed reason and still never reaches python3.
       (push-wave-commit! work "src/drift.rkt" "# drift\n" "source drift")
       (define refused (default-delivery-controller subject plan 0 "delivery-preflight"))
       (check-eq? (delivery-effect-result-kind refused) 'blocked)
       (define reason (hash-ref (delivery-effect-result-data refused) 'reason))
       (check-true (string-contains? reason "non-evidence paths") reason)
       (check-false (file-exists? marker) "a refused preflight must not spawn python3")))))
