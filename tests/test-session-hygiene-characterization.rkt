#lang racket

;; tests/test-session-hygiene-characterization.rkt
;;
;; CHARACTERIZATION — post-W3 (BUG-0038 flip; BUG-0033 half retained).
;;
;; BUG-0038 (FLIPPED by W3): the tracked-file write seam
;; (extensions/racket-tooling-helpers.rkt:write-file-string!) now routes
;; through runtime/session/tracked-write-hygiene.rkt:
;;   * write-time staleness guard — running (q-version) vs a FRESH read of
;;     util/version.rkt; divergence refuses the write with an error naming
;;     the PID and both versions ("stale process must not modify tracked
;;     files; restart q"). Repo-tracked scope only: paths outside the
;;     checkout stay writable.
;;   * escape hatch — current-allow-stale-tracked-writes bypasses for
;;     legitimate tooling and announces the override loudly (observer log).
;;   * concurrent-session detection — PID registry under the system temp
;;     dir; concurrent-writer-warning-once! fires exactly once per process.
;;   * idle demotion — session.idle-demote-hours (default 12) without user
;;     input => read-only until note-user-activity!.
;; These tests pin that behavior (all PASS now that W3 landed).
;;
;; BUG-0033 (NOT yet fixed — different wave): the project-root test-runner
;; shim <project-base>/scripts/run-tests.rkt still `(require "run-tests/...")`
;; submodules that exist ONLY under q/. Its absence pins remain UNFLIPPED.
;;
;; Pure-level: temp fake checkouts + source-surface scans; NO live
;; TUI/worker subprocess.

(require racket/file
         racket/format
         racket/path
         rackunit
         rackunit/text-ui
         (prefix-in hy: "../runtime/session/tracked-write-hygiene.rkt")
         (prefix-in seam: "../extensions/racket-tooling-helpers.rkt")
         (only-in "../util/lockfile.rkt" getpid))

(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
(define repo (simplify-path (build-path this-file 'up 'up)))
(define project-base (simplify-path (build-path repo 'up)))
(define (repo-file . parts)
  (apply build-path (cons repo parts)))
(define (base-file . parts)
  (apply build-path (cons project-base parts)))

(define SEAM-SOURCE (repo-file "extensions" "racket-tooling-helpers.rkt"))
(define HYGIENE-SOURCE (repo-file "runtime" "session" "tracked-write-hygiene.rkt"))
(define WRITE-PATH-SOURCES
  (list (repo-file "extensions" "racket-tooling-handlers.rkt")
        (repo-file "extensions" "racket-tooling.rkt")
        (repo-file "extensions" "hooks.rkt")
        (repo-file "extensions" "tool-api.rkt")
        (repo-file "extensions" "quarantine.rkt")))

(define (source-exists? p)
  (and (file-exists? p) #t))

(define (scan patterns files)
  (for*/list ([path (in-list files)]
              #:when (source-exists? path)
              [rx (in-list patterns)]
              #:when (regexp-match? rx (file->string path)))
    (cons rx (path->string path))))

;; Fake checkout with a util/version.rkt declaring VERSION. The guard
;; compares the (parameterizable) running version against a FRESH read of
;; this file — no git, no network.
(define (make-fake-checkout [version "9.9.9-new"])
  (define root (make-temporary-file "hygiene-fake-checkout~a" 'directory))
  (make-directory* (build-path root "util"))
  (call-with-output-file (build-path root "util" "version.rkt")
                         (lambda (out) (fprintf out "(define q-version \"~a\")\n" version))
                         #:exists 'replace)
  root)

(define (with-checkout version thunk)
  (define root (make-fake-checkout version))
  (dynamic-wind (lambda () #f)
                (lambda ()
                  (parameterize ([hy:current-tracked-write-repo-root root])
                    (thunk root)))
                (lambda () (delete-directory/files root #:must-exist? #f))))

;; Runs thunk, returns the exn:fail message it raised (or #f if none).
(define (refusal-of thunk)
  (define captured (box #f))
  (with-handlers ([exn:fail? (lambda (e)
                               (set-box! captured (exn-message e))
                               (void))])
    (thunk))
  (unbox captured))

(define suite
  (test-suite "BUG-0038 post-W3 + BUG-0033 pins: write-time staleness guard live; root test-runner shim still broken"

    ;; ------------------------------------------------------------
    ;; BUG-0038 FLIPPED — the tracked-file write path now refuses stale writes.
    ;; ------------------------------------------------------------

    (test-case "stale-version tracked write is refused naming the PID and both versions"
      (with-checkout
       "9.9.9-new"
       (lambda (root)
         (define target (build-path root "tracked.rkt"))
         (define msg
           (refusal-of (lambda ()
                         (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"])
                           (seam:write-file-string! target "content")))))
         (check-not-false msg "stale tracked write must be refused, not silently done")
         (check-true (string-contains? msg "stale process must not modify tracked files")
                     (format "refusal must name the rule: ~a" msg))
         (check-true (string-contains? msg "restart q"))
         (check-true (string-contains? msg (format "~a" (getpid)))
                     "refusal must name THIS process's PID")
         (check-true (string-contains? msg "1.0.0-old") "running version named")
         (check-true (string-contains? msg "9.9.9-new") "checkout version named")
         (check-false (file-exists? target) "refused write must not touch disk"))))

    (test-case "guard seam: assert-fresh-tracked-write! raises the same refusal directly"
      (with-checkout
       "9.9.9-new"
       (lambda (root)
         (define msg
           (refusal-of (lambda ()
                         (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"])
                           (hy:assert-fresh-tracked-write! (build-path root "any-tracked.rkt"))))))
         (check-not-false msg)
         (check-true (string-contains? msg "stale process must not modify tracked files"))
         ;; tracked-write-staleness reports the divergence pair directly.
         (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"])
           (check-equal? (hy:tracked-write-staleness (build-path root "any-tracked.rkt"))
                         (list "1.0.0-old" "9.9.9-new")
                         "staleness check returns running/on-disk pair on divergence"))
         (check-false (hy:tracked-write-staleness (build-path (find-system-path 'temp-dir)
                                                              "outside-repo.rkt"))
                      "paths outside the checkout are never considered stale-write scope"))))

    (test-case "escape hatch: override proceeds and is announced loudly"
      (with-checkout
       "9.9.9-new"
       (lambda (root)
         (define target (build-path root "override-write.rkt"))
         (define announced '())
         (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"]
                        [hy:current-allow-stale-tracked-writes #t]
                        [hy:current-tracked-write-override-observer
                         (lambda (msg) (set! announced (cons msg announced)))])
           (seam:write-file-string! target "legitimate-tooling"))
         (check-equal? (file->string target) "legitimate-tooling" "override write lands")
         (check-equal? (length announced) 1 "exactly one announcement")
         (define msg (car announced))
         (check-true (string-contains? msg "TRACKED-WRITE OVERRIDE"))
         (check-true (string-contains? msg (format "~a" (getpid))) "override names PID")
         (check-true (string-contains? msg "1.0.0-old"))
         (check-true (string-contains? msg "9.9.9-new"))
         (check-true (string-contains? msg "restart q") "override reminder still urges restart"))))

    (test-case "scope: writes outside the repo and same-version writes stay untouched"
      (with-checkout "9.9.9-new"
                     (lambda (_root)
                       ;; Outside the checkout: no staleness refusal even under divergence.
                       (define outside (make-temporary-file "hygiene-outside~a"))
                       (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"])
                         (seam:write-file-string! outside "outside-repo")
                         (check-equal? (file->string outside) "outside-repo"))
                       (delete-file outside)
                       ;; Inside the checkout with MATCHING versions: clean write, no override noise.
                       (parameterize ([hy:current-tracked-write-running-version "9.9.9-new"]
                                      [hy:current-tracked-write-override-observer
                                       (lambda (_msg)
                                         (fail "clean write must not announce an override"))])
                         (hy:assert-fresh-tracked-write! (build-path _root "clean.rkt"))
                         (seam:write-file-string! (build-path _root "clean.rkt") "clean")
                         (check-equal? (file->string (build-path _root "clean.rkt")) "clean")))))

    (test-case "seam present: helpers choke point routes through the guard (absence pin flipped)"
      ;; W0 pinned that NO write-path source mentioned staleness. W3 landed:
      ;; the shared seam now calls assert-fresh-tracked-write! before writing.
      (check-true (and (file-exists? SEAM-SOURCE) #t) "seam source exists")
      (define seam-src (file->string SEAM-SOURCE))
      (check-true (regexp-match? #rx"assert-fresh-tracked-write!" seam-src)
                  "write-file-string! must consult the staleness guard")
      (check-true (regexp-match? #rx"tracked-write-hygiene" seam-src)
                  "seam requires the hygiene module")
      (check-true (and (file-exists? HYGIENE-SOURCE) #t) "guard module exists")
      (define guard-src (file->string HYGIENE-SOURCE))
      (check-true (regexp-match? #rx"stale process must not modify tracked files" guard-src)
                  "guard carries the refusal text")
      (check-true (regexp-match? #rx"current-allow-stale-tracked-writes" guard-src)
                  "guard carries the escape hatch")
      ;; The write-path handler modules route their writes through the seam.
      (define rewrite (repo-file "extensions" "racket-tooling" "rewrite.rkt"))
      (check-true (and (file-exists? rewrite)
                       (regexp-match? #rx"write-file-string!" (file->string rewrite)))
                  "rewrite handler writes via the guarded seam"))

    (test-case "freshness asymmetry resolved: /go entry AND write path are both guarded"
      ;; W0 pinned the asymmetry as the defect; W3 closed it.
      (define go-src (repo-file "extensions" "gsd" "go-orchestrator.rkt"))
      (check-true (and go-src (file-exists? go-src)))
      (check-true (regexp-match? #rx"freshness" (file->string go-src))
                  "/go entry freshness guard still present (BUG-0031)")
      (check-true (regexp-match? #rx"assert-fresh-tracked-write!" (file->string SEAM-SOURCE))
                  "write path now guarded too — asymmetry closed"))

    (test-case "stale-session write no longer round-trips verbatim (durable pin flipped)"
      ;; W0 pinned that a stale-context write landed verbatim. Now the seam
      ;; refuses BEFORE disk is touched; only the explicit override lands.
      (with-checkout
       "9.9.9-new"
       (lambda (root)
         (define target (build-path root "tracked.rkt"))
         (define msg
           (refusal-of (lambda ()
                         (parameterize ([hy:current-tracked-write-running-version "1.0.0-old"])
                           (seam:write-file-string! target "stale in-memory content")))))
         (check-not-false msg "write refused")
         (check-false (file-exists? target) "nothing landed on disk"))))

    ;; ------------------------------------------------------------
    ;; Concurrent-session detection: second session announced once.
    ;; ------------------------------------------------------------

    (test-case "second-session detection fires once per process"
      (define pid-dir (make-temporary-file "hygiene-pids~a" 'directory))
      (dynamic-wind (lambda () #f)
                    (lambda ()
                      (parameterize ([hy:current-q-pid-dir pid-dir])
                        (hy:reset-concurrent-writer-warning!)
                        (hy:register-q-process!)
                        ;; A live foreign q process: PID 1 (init) is alive and is not us.
                        (call-with-output-file (build-path pid-dir "1.pid")
                                               (lambda (out) (fprintf out "0 1.0.0-old /nowhere\n"))
                                               #:exists 'replace)
                        (check-equal? (hy:concurrent-q-processes)
                                      (list 1)
                                      "foreign live PID detected, self excluded")
                        (define msg1 (hy:concurrent-writer-warning-once!))
                        (check-not-false msg1 "warning fires when another live q exists")
                        (check-true (string-contains? msg1 "concurrent-writer warning"))
                        (check-true (string-contains? msg1 "1") "names the other PID")
                        (check-false (hy:concurrent-writer-warning-once!)
                                     "warning fires exactly once per process")
                        ;; Leaving the session removes us from the registry.
                        (hy:unregister-q-process!)
                        (check-false (file-exists? (build-path pid-dir (format "~a.pid" (getpid))))
                                     "unregister removes our PID file")))
                    (lambda () (delete-directory/files pid-dir #:must-exist? #f))))

    ;; ------------------------------------------------------------
    ;; Idle demotion: read-only until touched.
    ;; ------------------------------------------------------------

    (test-case "idle demotion disables tracked writes until user activity"
      (with-checkout
       "9.9.9-new"
       (lambda (root)
         (parameterize ([hy:current-tracked-write-running-version "9.9.9-new"])
           (hy:reset-session-hygiene-state!)
           (check-equal? (hy:maybe-idle-demote! #:now-ms 1000.0)
                         'not-idle
                         "no activity yet => nothing to demote")
           (hy:note-user-activity!)
           (define t0 (current-inexact-milliseconds))
           ;; 11.9h idle: still writable; 12h: demoted.
           (check-equal? (hy:maybe-idle-demote! #:now-ms (+ t0 (* 11.9 3600 1000))) 'not-idle)
           (check-equal? (hy:maybe-idle-demote! #:now-ms (+ t0 (* 12 3600 1000))) 'demoted)
           (check-true (hy:session-idle-readonly?))
           (check-equal? (hy:maybe-idle-demote! #:now-ms (+ t0 (* 13 3600 1000)))
                         'already-demoted
                         "no repeated demotion noise")
           ;; Demoted session: tracked write refused even with matching versions.
           (define target (build-path root "auto-write.rkt"))
           (define msg (refusal-of (lambda () (seam:write-file-string! target "auto"))))
           (check-not-false msg "idle session must not auto-write")
           (check-true (string-contains? msg "idle session is read-only"))
           (check-true (string-contains? msg "session.idle-demote-hours")
                       "message names the responsible settings key")
           ;; User interacts: session reactivates and writes flow again.
           (hy:note-user-activity!)
           (check-false (hy:session-idle-readonly?))
           (seam:write-file-string! target "after-touch")
           (check-equal? (file->string target) "after-touch")
           (hy:reset-session-hygiene-state!)))))

    (test-case "settings key session.idle-demote-hours: nested/flat spellings, default 12"
      (check-equal? (hy:settings-idle-demote-hours #f) 12.0 "no settings => default")
      (check-equal? (hy:settings-idle-demote-hours (hash)) 12.0)
      (check-equal? (hy:settings-idle-demote-hours (hash 'session (hash 'idle-demote-hours 3)))
                    3.0
                    "nested spelling")
      (check-equal? (hy:settings-idle-demote-hours (hash "session.idle-demote-hours" 0.5))
                    0.5
                    "flat string spelling")
      (check-equal? (hy:settings-idle-demote-hours (hash 'session
                                                         (hash 'idle-demote-hours "garbage")))
                    12.0
                    "garbage tolerated => default")
      (hy:set-idle-demote-hours! 6)
      (check-equal? (hy:idle-demote-hours) 6.0)
      (hy:set-idle-demote-hours! 0)
      (check-equal? (hy:idle-demote-hours) 12.0 "zero threshold => default")
      (hy:set-idle-demote-hours! -1)
      (check-equal? (hy:idle-demote-hours) 12.0 "invalid threshold => default")
      (hy:reset-session-hygiene-state!))

    ;; ------------------------------------------------------------
    ;; BUG-0033 — project-root test-runner shim still requires missing submodules.
    ;; (NOT this wave's bug; pins retained until BUG-0033's wave flips them.)
    ;; ------------------------------------------------------------

    (test-case "root shim still requires run-tests submodules that do not exist at project root"
      (define root-shim (base-file "scripts" "run-tests.rkt"))
      (check-true (and root-shim (file-exists? root-shim))
                  "precondition: duplicate root shim still exists today")
      (define shim-src (file->string root-shim))
      (check-true
       (regexp-match? #rx"run-tests/classify[.]rkt" shim-src)
       "root shim still (require \"run-tests/classify.rkt\") — BUG-0033's wave will repair the shim situation and flips this pin")
      (define root-submodule-dir (base-file "scripts" "run-tests"))
      (check-false (and (directory-exists? root-submodule-dir) #t)
                   "precondition: scripts/run-tests/ still absent at project root (shim cannot load)")
      (define real-runner (repo-file "scripts" "run-tests.rkt"))
      (check-true (and real-runner (file-exists? real-runner))
                  "precondition: the real runner lives under q/scripts/ and works"))))

(module+ main
  (exit (run-tests suite)))
