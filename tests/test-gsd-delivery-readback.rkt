#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/delivery-handoff.rkt scripts/gsd-delivery-status.rkt
;; @timeout 600
;;
;; Phase B slice S10 — the LAST Python `status` path is gone.
;;
;; `delivery-readback` (extensions/gsd/delivery-handoff.rkt) used to shell
;; `python3 scripts/gsd-delivery.py status --campaign-root … --repo … --plan …
;; --wave …`. It now calls the native proof ported in
;; scripts/gsd-delivery-status.rkt (`delivery-status!`) with the SAME arguments,
;; and keeps the same contract:
;;
;;   * the payload the reader returns is the controller's own JSON object — a
;;     delivered proof is passed through verbatim, a refusal becomes
;;     {'status':'delivery-pending','reason':<reason>} whose reason text is the
;;     byte-identical typed-stop string the Python exit-2 payload carried;
;;   * the default reader is what every production caller gets, and the
;;     `#:delivery-reader` injection point still overrides it in tests;
;;   * no python3 process is spawned by the readback at all.
;;
;; Proof layout. Case (a) installs a `python3` PATH shim that writes a marker
;; when it runs (with a CONTROL invocation proving the shim is live) and shows
;; the default reader never touches it. Case (b) builds ONE real git world (bare
;; origin + implementation branch + protected squash merge + a schema-2 binding
;; trio published as the single-parent protected binding commit) with a fake
;; `gh`, so the native proof really proves delivery and the reader returns the
;; delivered payload — compared against the recorded Python key set, and against
;; the Python controller itself when python3 is available (differential). Case
;; (c) compares the native refusal reason with the Python controller's on the
;; SAME fixture. Case (d) shows an injected reader still wins over the native
;; default.

(require rackunit
         json
         openssl
         file/sha1
         racket/file
         racket/hash
         racket/list
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/campaign-state.rkt")

;; Resolve the interpreter BEFORE any fixture PATH is installed, so the fixture
;; PATH can keep the real racket (the strict gate is spawned through it).
(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory (path->string (path-only (path->complete-path racket-executable))))

(define-runtime-path python-controller "../scripts/gsd-delivery.py")

(define slug "coinerd/s10")
(define plan-id (make-string 64 #\a))
(define wave 1)
(define wave-branch (format "campaign/~a/w1" (substring plan-id 0 8)))
(define binding-name (format "binding/~a-w1" (substring plan-id 0 12)))
(define evidence-relative (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan-id))
(define review-relative (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
(define validation-relative (format "docs/reports/gsd-wave-validation/~a-w1.rktd" plan-id))
(define empty-digest (bytes->hex-string (sha256-bytes #"")))
(define governance-check "gsd-governance")
(define actions-app-id 15368)

;; The EXACT reason scripts/gsd-delivery.py's exit-2 payload carried for a wave
;; with no authoritative coordinator journal (status() -> journal_receipt()).
(define missing-journal-reason "delivery proof requires the authoritative coordinator journal")

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (fixture-env root [path "/usr/bin:/bin"])
  (define env (make-environment-variables))
  (for ([pair (in-list `(("PATH" . ,path) ("HOME" . ,(path->string root))
                                          ("GIT_CONFIG_NOSYSTEM" . "1")
                                          ("GIT_CONFIG_GLOBAL" . "/dev/null")
                                          ("GIT_TERMINAL_PROMPT" . "0")
                                          ("GIT_ALLOW_PROTOCOL" . "file")
                                          ("GIT_AUTHOR_NAME" . "Test")
                                          ("GIT_AUTHOR_EMAIL" . "test@example.invalid")
                                          ("GIT_COMMITTER_NAME" . "Test")
                                          ("GIT_COMMITTER_EMAIL" . "test@example.invalid")))])
    (environment-set! env (car pair) (cdr pair)))
  env)

(define (put path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; `.rktd` artifacts are ONE Racket hash datum (the trusted writer
;; scripts/gsd-binding-data.rkt pretty-writes a JSON hash).
(define (put-datum path datum)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (pretty-write datum out))))

(define (json-text value)
  (jsexpr->string value))

;; ============================================================
;; Real schema-2 trio fixtures (the unchanged strict gate must accept them)
;; ============================================================

(define (trio-evidence #:merge merge #:head head #:implementation implementation #:method method)
  (hasheq 'schema-version
          2
          'milestone
          1
          'issue
          7
          'wave
          "W1"
          'plan-id
          plan-id
          'status
          "ready-for-merge"
          'merge-method
          method
          'merge-sha
          merge
          'delivery-head-sha
          head
          'delivery-pr
          1
          'wave-branch
          wave-branch
          'branch
          wave-branch
          'merged-at
          "2026-01-01T00:00:00Z"
          'required-pr-checks
          '("ci")
          'binding-generation
          0
          'implementation-sha
          implementation
          'content-digest
          empty-digest
          'required-checks
          '("ci")
          'review-artifact
          review-relative
          'validation-artifact
          validation-relative))

(define (trio-review implementation)
  (hasheq
   'reviewer
   "independent-reviewer"
   'verdict
   "APPROVED"
   'timestamp
   "2026-01-01T00:00:00Z"
   'reviewed-sha
   implementation
   'content-digest
   empty-digest
   'scope
   "Independent review of the wave diff, the excluded-content digest and the strict gate"
   'report
   "The reviewer read the diff, recomputed the excluded digest and reran the strict trio gate."))

(define (trio-validation implementation)
  (hasheq 'status
          "current"
          'milestone
          1
          'wave
          "W1"
          'issue
          7
          'branch
          wave-branch
          'implementation-sha
          implementation
          'content-digest
          empty-digest
          'red-first
          (hasheq 'command
                  "racket tests/test-fixture-gate.rkt"
                  'failure
                  "the fixture gate failed before the implementation landed")
          'focused-tests
          (hasheq 'result "passed" 'command "raco test tests/test-fixture-gate.rkt")
          'format-compile
          (hasheq 'result "passed" 'command "raco make")
          'lint
          (hasheq 'result "passed" 'command "raco make lint")
          'fast
          (hasheq 'result "passed" 'command "racket scripts/run-tests.rkt --suite fast")
          'review-artifact
          review-relative
          'remaining-items
          '()
          'planning-sync
          "current"))

(define (trio-datums #:merge merge #:head head #:implementation implementation)
  (values (trio-evidence #:merge merge #:head head #:implementation implementation #:method "squash")
          (trio-review implementation)
          (trio-validation implementation)))

;; ============================================================
;; The real git world (built ONCE, cached for the run)
;; ============================================================

;; The trio datums are computed from the phase-1 facts (base/head/squash), so
;; they are written BETWEEN the two bounded build scripts.
(define (write-fixtures! root facts)
  (define head (hash-ref facts 'head))
  (define squash (hash-ref facts 'squash))
  (define-values (evidence review validation)
    (trio-datums #:merge squash #:head head #:implementation squash))
  (define dir (build-path root "fixtures"))
  (make-directory* dir)
  (put-datum (build-path dir "evidence") evidence)
  (put-datum (build-path dir "review") review)
  (put-datum (build-path dir "validation") validation)
  (hasheq 'evidence
          evidence
          'review
          review
          'validation
          validation
          'base
          (hash-ref facts 'base)
          'head
          head
          'squash
          squash))

(define (script-text lines)
  (string-append "#!/bin/sh\nset -e\n" (string-join lines "\n") "\n"))

(define (run-script! root name lines)
  (define path (build-path root name))
  (put path (script-text lines))
  (define result
    (run-subprocess "/bin/sh"
                    #:args (list (path->string path))
                    #:directory root
                    #:environment (current-environment-variables)
                    #:timeout 180
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-world "~a failed: ~a" name (subprocess-result-stderr result))))

(define (fact-key name)
  (case (string-downcase name)
    [("pub") 'publication]
    [else (string->symbol (string-downcase name))]))

(define (read-facts path)
  (define lines
    (if (file-exists? path)
        (file->lines path)
        '()))
  (for/fold ([acc (hash)]) ([line (in-list lines)])
    (define parts (string-split line "="))
    (hash-set acc (fact-key (car parts)) (cadr parts))))

(define script-preamble
  (list "export GIT_AUTHOR_NAME=Test"
        "export GIT_AUTHOR_EMAIL=test@example.invalid"
        "export GIT_COMMITTER_NAME=Test"
        "export GIT_COMMITTER_EMAIL=test@example.invalid"
        "export GIT_CONFIG_NOSYSTEM=1"
        "export GIT_CONFIG_GLOBAL=/dev/null"
        "export GIT_ALLOW_PROTOCOL=file"
        "ROOT=$(pwd)"))

;; Phase 1: origin + the implementation branch + the protected squash merge.
(define (build-world-phase-1! root)
  (run-script!
   root
   "build-1.sh"
   (append script-preamble
           (list "git init -q --bare -b main origin.git"
                 "git init -q -b main work"
                 "cd work"
                 "printf 'initial\\n' > tracked"
                 "mkdir -p scripts"
                 "printf '(\"ci\")\\n' > scripts/required-pr-checks.policy"
                 "git add -A"
                 "git commit -q -m 'initial policy'"
                 "BASE=$(git rev-parse HEAD)"
                 "git remote add origin ../origin.git"
                 (format "git checkout -q -b ~a \"$BASE\"" wave-branch)
                 "printf 'impl\\n' > feature"
                 "git add feature"
                 "git commit -q -m impl"
                 "HEAD_SHA=$(git rev-parse HEAD)"
                 (format "git push -q origin ~a" wave-branch)
                 (format "git push -q origin \"$HEAD_SHA:refs/pull/1/head\"")
                 "git checkout -q main"
                 (format "git merge -q --squash ~a" wave-branch)
                 "git commit -q -m 'squash merge'"
                 "SQUASH=$(git rev-parse HEAD)"
                 "HEAD_TREE=$(git rev-parse \"$HEAD_SHA^{tree}\")"
                 (string-append
                  "printf 'BASE=%s\\nHEAD=%s\\nSQUASH=%s\\nHEAD-TREE=%s\\n' "
                  "\"$BASE\" \"$HEAD_SHA\" \"$SQUASH\" \"$HEAD_TREE\" > \"$ROOT/facts-1.txt\"")))))

;; Phase 2: the binding publication and the subject clone at <root>/q (the
;; campaign root keeps the journal, the nested checkout is the repository the
;; readback resolves — exactly the two-tier layout of a real campaign).
(define (build-world-phase-2! root)
  (define evidence-dir (path->string (path-only (string->path evidence-relative))))
  (define review-dir (path->string (path-only (string->path review-relative))))
  (define validation-dir (path->string (path-only (string->path validation-relative))))
  (run-script! root
               "build-2.sh"
               (append script-preamble
                       (list "cd work"
                             "SQUASH=$(git rev-parse main)"
                             (format "mkdir -p ~a ~a ~a" evidence-dir review-dir validation-dir)
                             (format "cp ../fixtures/evidence ~a" evidence-relative)
                             (format "cp ../fixtures/review ~a" review-relative)
                             (format "cp ../fixtures/validation ~a" validation-relative)
                             "git add -A"
                             "git commit -q -m 'binding trio'"
                             "PUB=$(git rev-parse HEAD)"
                             "git push -q origin main"
                             "cd \"$ROOT\""
                             "git clone -q origin.git q"
                             "cd q"
                             "git config user.name Test"
                             "git config user.email test@example.invalid"
                             (format "git config remote.origin.url 'https://github.com/~a'" slug)
                             (format "git config 'url.~a.insteadOf' 'https://github.com/~a'"
                                     (path->string (build-path root "origin.git"))
                                     slug)
                             "printf 'PUB=%s\\n' \"$PUB\" > \"$ROOT/facts-2.txt\""))))

(define (build-world! root)
  (build-world-phase-1! root)
  (define facts-1 (read-facts (build-path root "facts-1.txt")))
  (define datums (write-fixtures! root facts-1))
  (build-world-phase-2! root)
  (define facts-2 (read-facts (build-path root "facts-2.txt")))
  (define facts
    (hash-union (hash-union datums facts-1 #:combine (lambda (a b) b))
                facts-2
                #:combine (lambda (a b) b)))
  (hash-set facts 'subject (build-path root "q")))

(define world-cache (make-hash))
(define world-lock (make-semaphore 1))

(define (shared-world)
  (call-with-semaphore world-lock
                       (lambda ()
                         (hash-ref! world-cache
                                    'delivered
                                    (lambda ()
                                      (define root
                                        (make-temporary-file "readback-world-~a" 'directory))
                                      (define env (fixture-env root "/usr/bin:/bin"))
                                      (parameterize ([current-environment-variables env])
                                        (check-not-false (find-executable-path "git")
                                                         "the fixture must resolve git through PATH")
                                        (build-world! root)))))))

;; ============================================================
;; Journal fixture (the authoritative receipt the proof authenticates)
;; ============================================================

(define (write-journal! root world)
  (define receipt
    (hasheq 'repo
            slug
            'branch
            wave-branch
            'origin
            (format "https://github.com/~a" slug)
            'evidence
            evidence-relative
            'head
            (hash-ref world 'head)
            'tree
            (hash-ref world 'head-tree)
            'verified-at
            1
            'attempt-id
            "attempt-7"
            'attempt-fence
            3))
  (define journal
    (hasheq 'schema-version 1 'plan-id plan-id 'wave wave 'stage "governance" 'receipt receipt))
  (put (build-path root ".planning" "campaigns" plan-id "coordinator-w1.json")
       (string-append (json-text journal) "\n")))

;; ============================================================
;; GitHub fixtures (the same routes the Python ladder reads)
;; ============================================================

(define (route slug* path)
  (format "repos/~a/~a" slug* path))
(define (check-runs-route sha)
  (format "commits/~a/check-runs?per_page=100&page=1" sha))

(define (merged-pr-json #:number number #:merge merge #:head-sha head-sha #:head-ref head-ref)
  (hasheq 'number
          number
          'state
          "closed"
          'merged
          #t
          'merged_at
          "2026-01-01T00:00:00Z"
          'merge_commit_sha
          merge
          'head
          (hasheq 'ref head-ref 'sha head-sha 'repo (hasheq 'full_name slug))
          'base
          (hasheq 'ref "main" 'sha (make-string 40 #\c) 'repo (hasheq 'full_name slug))))

(define (commit-json sha parents)
  (hasheq 'sha
          sha
          'parents
          (for/list ([parent (in-list parents)])
            (hasheq 'sha parent))))

(define (protection-json)
  (hasheq 'enforce_admins
          (hasheq 'enabled #t)
          'required_status_checks
          (hasheq 'strict #t 'contexts (list "ci"))
          'allow_force_pushes
          (hasheq 'enabled #f)
          'allow_deletions
          (hasheq 'enabled #f)))

(define (check-run-json #:name name #:sha sha #:run-id run-id)
  (hasheq 'id
          11
          'name
          name
          'status
          "completed"
          'conclusion
          "success"
          'head_sha
          sha
          'app
          (hasheq 'id actions-app-id)
          'details_url
          (format "https://github.com/~a/actions/runs/~a/job/9001" slug run-id)
          'workflow-path
          ".github/workflows/ci.yml"))

(define (workflow-run-json #:id id #:sha sha #:event event #:branch branch)
  (hasheq 'id
          id
          'status
          "completed"
          'conclusion
          "success"
          'head_sha
          sha
          'head_branch
          branch
          'event
          event
          'path
          ".github/workflows/ci.yml"
          'repository
          (hasheq 'full_name slug)))

(define (check-runs-json runs)
  (hasheq 'total_count (length runs) 'check_runs runs))

(define (status-fixtures world)
  (define head (hash-ref world 'head))
  (define publication (hash-ref world 'publication))
  (list
   (cons (route slug "pulls/1")
         (json-text (merged-pr-json #:number 1
                                    #:merge (hash-ref world 'squash)
                                    #:head-sha head
                                    #:head-ref wave-branch)))
   (cons (route slug (format "commits/~a" (hash-ref world 'squash)))
         (json-text (commit-json (hash-ref world 'squash) (list (hash-ref world 'base)))))
   (cons (route slug "branches/main/protection") (json-text (protection-json)))
   (cons (route slug (check-runs-route head))
         (json-text (check-runs-json (list (check-run-json #:name "ci" #:sha head #:run-id 1001)))))
   (cons (route slug "actions/runs/1001")
         (json-text
          (workflow-run-json #:id 1001 #:sha head #:event "pull_request" #:branch wave-branch)))
   (cons
    (route slug (format "commits/~a/pulls" publication))
    (json-text
     (list (merged-pr-json #:number 2 #:merge publication #:head-sha head #:head-ref binding-name))))
   (cons (route slug (check-runs-route publication))
         (json-text (check-runs-json (list (check-run-json #:name governance-check
                                                           #:sha publication
                                                           #:run-id 2002)))))
   (cons (route slug "actions/runs/2002")
         (json-text (workflow-run-json #:id 2002 #:sha publication #:event "push" #:branch "main")))))

(define (gh-shim-script routes)
  (string-append
   "#!/bin/sh\n"
   "route=\n"
   "for arg in \"$@\"; do case \"$arg\" in repos/*) route=$arg ;; esac; done\n"
   "case \"$route\" in\n"
   (string-join
    (append (for/list ([entry (in-list routes)])
              (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cdr entry)))
            (list "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;"))
    "\n")
   "\nesac\n"))

;; A `python3` shim that records every invocation in a marker file. Nothing in
;; the native readback may ever execute it.
(define (python3-shim-script marker)
  (format "#!/bin/sh\nprintf 'spawned\\n' >> ~s\nexit 97\n" (path->string marker)))

(define (install-shims! root fixtures #:python-shim? [python-shim? #f])
  (define bin (build-path root "bin"))
  (define fixtures-dir (build-path root "fixtures"))
  (make-directory* bin)
  (make-directory* fixtures-dir)
  (define routes
    (for/list ([entry (in-list fixtures)]
               [index (in-naturals)])
      (define file (format "fixture-~a.json" index))
      (put (build-path fixtures-dir file) (cdr entry))
      (cons (car entry) file)))
  (define gh (build-path bin "gh"))
  (put gh (gh-shim-script routes))
  (file-or-directory-permissions gh #o700)
  (define marker (build-path root "python3.marker"))
  (when python-shim?
    (define python (build-path bin "python3"))
    (put python (python3-shim-script marker))
    (file-or-directory-permissions python #o700))
  (define env (fixture-env root (format "~a:~a:/usr/bin:/bin" bin racket-directory)))
  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
  (values env marker))

;; The cached world's repository is IMMUTABLE, but a status proof runs `git
;; fetch` (it writes FETCH_HEAD and the tracking ref), so every case drives its
;; own copy at <case-root>/q — the two-tier layout the campaign root needs.
(define (copy-subject! world root)
  (define dst (build-path root "q"))
  (define result
    (run-subprocess "/bin/cp"
                    #:args (list "-a" (path->string (hash-ref world 'subject)) (path->string dst))
                    #:environment (current-environment-variables)
                    #:timeout 120
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-copy "subject copy failed: ~a" (subprocess-result-stderr result)))
  dst)

;; One case = its own campaign root (journal) + <root>/q repository + the shims.
(define (with-world proc #:python-shim? [python-shim? #f])
  (define cached (shared-world))
  (define root (make-temporary-file "readback-case-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (copy-subject! cached root)
                  (write-journal! root cached)
                  (define-values (env marker)
                    (install-shims! root (status-fixtures cached) #:python-shim? python-shim?))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "gh")
                                     "the fixture must resolve gh through PATH")
                    (proc cached root env marker)))
                (lambda () (delete-directory/files root))))

;; A campaign-root-only fixture (no repository at all): the readback cannot prove
;; anything, which is exactly the pending/refusal shape.
(define (with-bare-root proc #:python-shim? [python-shim? #f])
  (define root (make-temporary-file "readback-bare-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define-values (env marker) (install-shims! root '() #:python-shim? python-shim?))
                  (parameterize ([current-environment-variables env])
                    (proc root env marker)))
                (lambda () (delete-directory/files root))))

;; ============================================================
;; The Python controller, for differential parity (skipped without python3)
;; ============================================================

(define (python-status env campaign-root repo)
  (define python (find-executable-path "python3"))
  (and python
       (let ([result (run-subprocess (path->string python)
                                     #:args (list (path->string python-controller)
                                                  "status"
                                                  "--campaign-root"
                                                  (path->string (path->complete-path campaign-root))
                                                  "--repo"
                                                  (path->string (path->complete-path repo))
                                                  "--plan"
                                                  plan-id
                                                  "--wave"
                                                  (number->string wave))
                                     #:environment env
                                     #:timeout 240
                                     #:process-group? #t)])
         (string->jsexpr (subprocess-result-stdout result)))))

(define (delivered-proof merge-sha)
  (hasheq 'status "delivered" 'merge-sha merge-sha 'plan-id plan-id 'wave wave))

(module+ test
  ;; ============================================================
  ;; (a) the default reader spawns NO python3
  ;; ============================================================
  (test-case "default delivery readback never spawns python3 (PATH shim control)"
    (with-bare-root
     (lambda (root env marker)
       ;; CONTROL: the shim is live and on PATH, so an absent marker below is
       ;; evidence and not a broken fixture.
       (run-subprocess "python3"
                       #:args '("--version")
                       #:environment env
                       #:timeout 60
                       #:process-group? #t)
       (check-true (file-exists? marker) "the python3 PATH shim must record an invocation")
       (delete-file marker)
       (define result (delivery-readback root plan-id wave))
       (check-false (file-exists? marker)
                    "the native readback must not execute python3 (not even as a fallback)")
       (check-equal? result (hasheq 'status "delivery-pending" 'reason missing-journal-reason)))
     #:python-shim? #t))

  ;; ============================================================
  ;; (b) pending payload: the exact Python shape + reason
  ;; ============================================================
  (test-case "pending readback keeps the Python payload shape and reason text"
    (with-bare-root
     (lambda (root env marker)
       (define result (delivery-readback root plan-id wave))
       (check-equal? (sort (map symbol->string (hash-keys result)) string<?) '("reason" "status"))
       (check-equal? (hash-ref result 'status) "delivery-pending")
       (check-equal? (hash-ref result 'reason) missing-journal-reason)
       ;; Differential: the reason is byte-identical to the Python controller's
       ;; own exit-2 payload for the SAME fixture (skipped without python3).
       (define python (find-executable-path "python3"))
       (if python
           (let ([payload (python-status env root (build-path root "q"))])
             (check-equal? (hash-ref payload 'status) "delivery-pending")
             (check-equal? (hash-ref payload 'reason)
                           (hash-ref result 'reason)
                           "the native refusal must keep the Python reason text"))
           (printf "skip: python3 unavailable; differential reason check skipped\n")))))

  ;; ============================================================
  ;; (b) delivered payload: the exact Python shape, end to end
  ;; ============================================================
  (test-case "delivered readback returns the Python delivered payload verbatim"
    (with-world (lambda (world root env marker)
                  (define payload (delivery-readback root plan-id wave))
                  (define expected
                    (hasheq 'status
                            "delivered"
                            'merge-sha
                            (hash-ref world 'squash)
                            'publication-sha
                            (hash-ref world 'publication)
                            'plan-id
                            plan-id
                            'wave
                            wave
                            'delivery-head-sha
                            (hash-ref world 'head)
                            'delivery-branch
                            wave-branch
                            'binding-generation
                            0
                            'attempt-id
                            "attempt-7"
                            'attempt-fence
                            3))
                  (check-equal? payload expected "the delivered proof is passed through unchanged")
                  ;; The key set is EXACTLY the recorded Python one.
                  (check-equal? (sort (map symbol->string (hash-keys payload)) string<?)
                                '("attempt-fence" "attempt-id"
                                                  "binding-generation"
                                                  "delivery-branch"
                                                  "delivery-head-sha"
                                                  "merge-sha"
                                                  "plan-id"
                                                  "publication-sha"
                                                  "status"
                                                  "wave"))
                  ;; Differential: the Python controller proves the same delivery on the
                  ;; same repository and returns the same object (skipped without python3).
                  (define python (find-executable-path "python3"))
                  (if python
                      (let ([python-payload (python-status env root (build-path root "q"))])
                        (check-equal? python-payload
                                      expected
                                      "native and Python status must return the same payload"))
                      (printf "skip: python3 unavailable; differential delivered check skipped\n")))))

  ;; ============================================================
  ;; (d) the injectable reader still overrides the native default
  ;; ============================================================
  (test-case "an injected delivery reader still overrides the native default"
    (define dir (make-temporary-file "readback-inject-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define merge-sha (make-string 40 #\d))
       (define verified (make-hash))
       (define resolver
         (delivered-predecessor-resolver dir
                                         plan-id
                                         verified
                                         (lambda (_b _p _w) (delivered-proof merge-sha))))
       (check-equal? (resolver dir plan-id wave)
                     merge-sha
                     "the injected reader's delivered proof is used")
       (check-equal? (hash-ref verified wave) merge-sha "the injected proof is memoized")
       ;; A pending injected reader still gates: nothing is invented.
       (define pending-resolver
         (delivered-predecessor-resolver dir
                                         plan-id
                                         (make-hash)
                                         (lambda (_b _p _w)
                                           (hasheq 'status "delivery-pending" 'reason "pending"))))
       (check-false (pending-resolver dir plan-id wave))
       ;; The DEFAULT reader on the same journal-less root is pending, so the
       ;; values above can only come from the injection point.
       (check-false (verified-wave-merge-sha dir plan-id wave)))
     (lambda () (delete-directory/files dir #:must-exist? #f)))))
