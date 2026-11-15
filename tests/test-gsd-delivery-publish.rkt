#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-publish.rkt
;; @covers extensions/gsd/delivery-publication.rkt
;; @timeout 240
;; Phase B slice S11: the `publish-head` action of scripts/gsd-delivery.py is
;; ported to Racket (scripts/gsd-delivery-publish.rkt) and the delivery
;; publication owner (extensions/gsd/delivery-publication.rkt) now calls it
;; natively instead of shelling python3.
;;
;; Parity law under test (the Python check ORDER is the contract, so the first
;; failing proof is always the same one):
;;   nonblank branch -> protected branch -> check-ref-format normalization ->
;;   campaign/wave binding -> full-SHA head -> configured origin (and the
;;   approved expected-origin identity) -> approved head object -> local branch
;;   ref -> approved tree -> ONE exact remote readback -> ancestor-only
;;   fast-forward -> ONE non-force push -> guard re-verification, exact
;;   post-push readback, guard re-verification AGAIN.
;;
;; The fixtures are ONE cached git world (bare origin + work checkout on
;; campaign/aaaaaaaa/w0) copied per case; a `git` PATH shim in front of the
;; real executable logs every argv vector (so the exact command order and the
;; "exactly one push, never retried" invariant are observable) and injects the
;; fault of each refusal case. Every refusal assertion also proves NO mutation
;; was reachable: no push line in the argv log and the remote tip unchanged.
;;
;; Evidence binding / journal law: publish_head itself writes NO local file and
;; updates no journal — that stays the caller's duty. The last two cases pin
;; both halves: the production `default-approved-head-publisher` is native
;; (python3 is never spawned) and `publish-approved-head!` still records the
;; EXACT evidence/head/branch, while the action's own working tree is
;; untouched.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-publish.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-publication.rkt")

(define-runtime-path publish-script "../scripts/gsd-delivery-publish.rkt")
(define racket-exe
  (let ([found (find-executable-path "racket")])
    (if found
        (path->string found)
        "racket")))
;; The REAL git the shims delegate to (resolved before any PATH override).
(define real-git
  (let ([found (find-executable-path "git")])
    (unless found (error 'fixture "git is required for the publish-head fixtures"))
    (path->string found)))

;; ============================================================
;; Fixture environment (no ambient git/credential/transport config)
;; ============================================================

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
    (run-subprocess real-git
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

;; ============================================================
;; ONE cached world, copied per case
;; ============================================================

(define-struct world (root work origin head tree branch plan) #:transparent)

(define (world-origin-url w) (path->string (world-origin w)))

(define (build-template!)
  (define root (make-temporary-file "publish-template-~a" 'directory))
  (parameterize ([current-environment-variables (fixture-env root)])
    (define origin (build-path root "origin.git"))
    (define work (build-path root "work"))
    (git root "init" "--bare" (path->string origin))
    (git root "init" "-b" "main" (path->string work))
    (put (build-path work "README.md") "base\n")
    (git work "add" "README.md")
    (git work "commit" "-m" "base")
    (git work "remote" "add" "origin" (path->string origin))
    (git work "push" "origin" "main")
    (define branch "campaign/aaaaaaaa/w0")
    (git work "checkout" "-q" "-b" branch "main")
    (put (build-path work "wave.txt") "approved\n")
    (git work "add" "wave.txt")
    (git work "commit" "-m" "wave commit")
    (define head (git work "rev-parse" "HEAD"))
    (define tree (git work "rev-parse" (string-append head "^{tree}")))
    (world root work origin head tree branch (make-string 64 #\a))))

;; ONE cached world per process (built once, then copied per case).
(define template-world (box #f))

(define (world-template)
  (or (unbox template-world)
      (let ([built (build-template!)])
        (set-box! template-world built)
        built)))

;; Copy the cached world into a fresh case root. The copied repository's
;; origin URL is re-pointed at the COPIED bare origin (absolute paths do not
;; survive a directory copy).
(define (with-world proc)
  (define case-root (make-temporary-file "publish-case-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define case-world (build-path case-root "world"))
     (define template (world-template))
     (copy-directory/files (world-root template) case-world)
     (define work (build-path case-world "work"))
     (define origin (build-path case-world "origin.git"))
     (parameterize ([current-environment-variables (fixture-env case-root)])
       (git work "config" "remote.origin.url" (path->string origin))
       (proc (world case-root
                    work
                    origin
                    (world-head template)
                    (world-tree template)
                    (world-branch template)
                    (world-plan template)))))
   (lambda () (delete-directory/files case-root))))

;; ============================================================
;; PATH shims: log every argv vector, optionally inject a fault
;; ============================================================

(define (install-shim! root script)
  (define bin (build-path root "shim-bin"))
  (make-directory* bin)
  (define shim (build-path bin "git"))
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
                  (when old
                    (environment-variables-set! env #"PATH" old)))))

;; A logging shim that delegates everything to the real git.
(define (logging-shim log)
  (format "#!/bin/sh\nprintf '%s\\n' \"$*\" >> '~a'\nexec '~a' \"$@\"\n" log real-git))

;; Run `proc` with a logging shim in front of git. Returns the argv log
;; (list of argument vectors) captured during the call.
(define (with-logging-git w proc)
  (define log (build-path (world-root w) "argv.log"))
  (define bin (install-shim! (world-root w) (logging-shim log)))
  (with-path bin
             (lambda ()
               (proc)
               (if (file-exists? log)
                   (for/list ([line (in-list (file->lines log))])
                     (string-split line))
                   '()))))

(define (argv->line args)
  (string-join args " "))

;; The exact happy-path argv order: full proof, ONE push, then the full
;; guard/readback/guard re-verification.
(define (expected-publish-argv w)
  (define r (path->string (world-work w)))
  (define b (world-branch w))
  (define h (world-head w))
  (define t (world-tree w))
  (define remote-ref (string-append "refs/heads/" b))
  (list (list "-C" r "check-ref-format" "--branch" b)
        (list "-C" r "config" "--get" "remote.origin.url")
        (list "-C" r "rev-parse" (string-append h "^{commit}"))
        (list "-C" r "rev-parse" remote-ref)
        (list "-C" r "rev-parse" (string-append h "^{tree}"))
        (list "-C" r "ls-remote" "--heads" "origin" remote-ref)
        (list "-C" r "push" "--no-tags" "origin" (string-append h ":" remote-ref))
        (list "-C" r "config" "--get" "remote.origin.url")
        (list "-C" r "rev-parse" remote-ref)
        (list "-C" r "rev-parse" (string-append h "^{tree}"))
        (list "-C" r "ls-remote" "--heads" "origin" remote-ref)
        (list "-C" r "config" "--get" "remote.origin.url")
        (list "-C" r "rev-parse" remote-ref)
        (list "-C" r "rev-parse" (string-append h "^{tree}"))))

;; The already-published argv order: the same guards, ONE readback, the
;; idempotent re-verification, and NO push.
(define (expected-already-published-argv w)
  (define r (path->string (world-work w)))
  (define b (world-branch w))
  (define h (world-head w))
  (define t (world-tree w))
  (define remote-ref (string-append "refs/heads/" b))
  (list (list "-C" r "check-ref-format" "--branch" b)
        (list "-C" r "config" "--get" "remote.origin.url")
        (list "-C" r "rev-parse" (string-append h "^{commit}"))
        (list "-C" r "rev-parse" remote-ref)
        (list "-C" r "rev-parse" (string-append h "^{tree}"))
        (list "-C" r "ls-remote" "--heads" "origin" remote-ref)
        (list "-C" r "config" "--get" "remote.origin.url")
        (list "-C" r "rev-parse" remote-ref)
        (list "-C" r "rev-parse" (string-append h "^{tree}"))))

(define (pushes log)
  (for/list ([args (in-list log)] #:when (member "push" args)) args))

;; Read a logging-shim argv log back into argument vectors.
(define (logged-argv log)
  (if (file-exists? log)
      (for/list ([line (in-list (file->lines log))]) (string-split line))
      '()))

(define (has-command? log name)
  (and (for/or ([args (in-list log)]) (member name args)) #t))

(define (refusal-message thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (error 'test "expected a typed delivery refusal, got success")))

(define missing (gensym))

;; The action under test, with per-case overrides.
(define (publish! w
                  #:branch [branch missing]
                  #:head [head missing]
                  #:expected-origin [expected-origin missing]
                  #:expected-tree [expected-tree missing]
                  #:plan [plan missing]
                  #:wave [wave missing])
  (delivery-publish-head! (world-work w)
                          (if (eq? branch missing) (world-branch w) branch)
                          (if (eq? head missing) (world-head w) head)
                          #:expected-origin (if (eq? expected-origin missing)
                                                (world-origin-url w)
                                                expected-origin)
                          #:expected-tree (if (eq? expected-tree missing)
                                              (world-tree w)
                                              expected-tree)
                          #:plan (if (eq? plan missing) (world-plan w) plan)
                          #:wave (if (eq? wave missing) 0 wave)))

(define (remote-tip w)
  (define out (git (world-work w) "ls-remote" "--heads" (world-origin-url w)
                   (string-append "refs/heads/" (world-branch w))))
  (and (not (string=? out "")) (car (string-split out))))

(define (run-cli root . args)
  (run-subprocess racket-exe
                  #:args (append (list (path->string publish-script)) args)
                  #:directory root
                  #:environment (current-environment-variables)
                  #:timeout 120
                  #:process-group? #t))

(define (cli-args w)
  (list "publish-head"
        "--repo" (path->string (world-work w))
        "--plan" (world-plan w)
        "--wave" "0"
        "--expected-branch" (world-branch w)
        "--expected-head" (world-head w)
        "--expected-origin" (world-origin-url w)
        "--expected-tree" (world-tree w)))

;; Remove a flag AND its value from a CLI argv list.
(define (except-flag args flag)
  (cond
    [(null? args) '()]
    [(equal? (car args) flag) (if (null? (cdr args)) '() (except-flag (cddr args) flag))]
    [else (cons (car args) (except-flag (cdr args) flag))]))

;; Replace the value that follows a flag in a CLI argv list.
(define (set-flag-value args flag value)
  (cond
    [(null? args) (list flag value)]
    [(equal? (car args) flag) (cons flag (cons value (cddr args)))]
    [else (cons (car args) (set-flag-value (cdr args) flag value))]))

(module+ test
  ;; ----------------------------------------------------------------
  ;; Happy path: exact payload + exact argv order + one write
  ;; ----------------------------------------------------------------
  (test-case "happy path publishes the exact approved head with the exact argv order"
    (with-world
     (lambda (w)
       (check-false (remote-tip w) "the fixture starts unpublished")
       (define head-before (git (world-work w) "rev-parse" "HEAD"))
       (define log
         (with-logging-git
          w
          (lambda ()
            (define result (publish! w))
            (check-equal? result
                          (hasheq 'status "published"
                                  'branch (world-branch w)
                                  'head (world-head w))))))
       ;; byte-exact command order
       (check-equal? (map argv->line log)
                     (map argv->line (expected-publish-argv w)))
       ;; exactly ONE push, after the complete pre-publication proof chain
       (check-equal? (length (pushes log)) 1)
       (check-true (has-command? log "ls-remote"))
       (check-equal? (remote-tip w) (world-head w))
       ;; no local mutation and no local file written by the action
       (check-equal? (git (world-work w) "rev-parse" "HEAD") head-before)
       (check-equal? (git (world-work w) "status" "--porcelain") "")
       (check-false (directory-exists? (build-path (world-work w) ".planning"))))))

  (test-case "already-published is idempotent: one readback, zero writes"
    (with-world
     (lambda (w)
       ;; A prior attempt's effect is already on the origin.
       (git (world-work w) "push" "origin"
            (string-append (world-head w) ":refs/heads/" (world-branch w)))
       (define log
         (with-logging-git
          w
          (lambda ()
            (check-equal? (publish! w)
                          (hasheq 'status "already-published"
                                  'branch (world-branch w)
                                  'head (world-head w))))))
       (check-equal? (map argv->line log)
                     (map argv->line (expected-already-published-argv w)))
       (check-equal? (pushes log) '())
       (check-equal? (remote-tip w) (world-head w)))))

  ;; ----------------------------------------------------------------
  ;; Unapproved head / wrong source identity / wrong branch-base
  ;; ----------------------------------------------------------------
  (test-case "an unapproved head refuses before any remote read and never pushes"
    (with-world
     (lambda (w)
       (define unapproved (make-string 40 #\f))
       (define log
         (with-logging-git
          w
          (lambda ()
            (define reason (refusal-message (lambda () (publish! w #:head unapproved))))
            (check-true (string-contains? reason "unknown-ref") reason)
            (check-true (string-contains? reason "git") reason))))
       ;; the local-object proof failed BEFORE the remote readback/push
       (check-false (has-command? log "ls-remote"))
       (check-equal? (pushes log) '())
       (check-false (remote-tip w)))))

  (test-case "wrong source identity (origin/tree) and wrong ref/base refuse with the Python reasons"
    (with-world
     (lambda (w)
       (define branch (world-branch w))
       (define head (world-head w))
       (define tree (world-tree w))
       ;; expected origin identity drifted
       (check-equal?
        (refusal-message
         (lambda ()
           (publish! w #:expected-origin (string-append (world-origin-url w) "-other"))))
        "origin drifted before publication")
       ;; expected tree identity drifted
       (check-equal? (refusal-message (lambda () (publish! w #:expected-tree (make-string 40 #\c))))
                     "approved tree drifted before publication")
       ;; a real object that is NOT the local branch tip: wrong base/ref
       (git (world-work w) "checkout" "-q" "main")
       (define main-head (git (world-work w) "rev-parse" "HEAD"))
       (git (world-work w) "checkout" "-q" branch)
       (check-equal? (refusal-message (lambda () (publish! w #:head main-head)))
                     "local branch ref drifted before publication")
       ;; branch/campaign-wave binding drift
       (check-equal?
        (refusal-message (lambda () (publish! w #:wave 1)))
        "publish-head branch is not bound to campaign/wave")
       (check-equal? (refusal-message (lambda () (publish! w #:plan "not-a-plan")))
                     "publish-head requires exact campaign plan id")
       (check-equal? (refusal-message (lambda () (publish! w #:wave -1)))
                     "publish-head requires exact wave index")
       ;; protected / malformed / short identity refusals
       (check-equal? (refusal-message (lambda () (publish! w #:branch "main")))
                     "publish-head refuses protected branch main")
       (check-equal? (refusal-message (lambda () (publish! w #:branch "master")))
                     "publish-head refuses protected branch master")
       (check-equal? (refusal-message (lambda () (publish! w #:branch "refs/heads/campaign/x")))
                     "publish-head refuses malformed branch refs/heads/campaign/x")
       (check-equal? (refusal-message (lambda () (publish! w #:branch "")))
                     "publish-head requires --expected-branch")
       (check-equal? (refusal-message (lambda () (publish! w #:head (substring head 0 12))))
                     "publish-head requires --expected-head")
       (check-equal? tree (world-tree w))
       ;; nothing was ever published by any refusal above
       (check-false (remote-tip w)))))

  (test-case "a missing configured origin is the typed command failure, never a publish"
    (with-world
     (lambda (w)
       (git (world-work w) "config" "--unset" "remote.origin.url")
       (define log
         (with-logging-git
          w
          (lambda ()
            (define reason (refusal-message (lambda () (publish! w))))
            (check-true (string-contains? reason "delivery command failed") reason))))
       (check-equal? (pushes log) '())
       (check-false (has-command? log "ls-remote")))))

  ;; ----------------------------------------------------------------
  ;; Protected-commit / ancestry rules: ancestor fast-forward only
  ;; ----------------------------------------------------------------
  (test-case "an ancestor remote tip fast-forwards; divergence refuses with no push"
    (with-world
     (lambda (w)
       (define branch (world-branch w))
       (define head (world-head w))
       ;; The remote tip is an ANCESTOR of the approved head: fast-forward.
       (git (world-work w) "push" "origin" (string-append "main:refs/heads/" branch))
       (define log
         (with-logging-git
          w
          (lambda ()
            ;; legacy/unbound direct-helper shape (no campaign binding args)
            (check-equal? (publish! w #:plan #f #:wave #f)
                          (hasheq 'status "published" 'branch branch 'head head)))))
       (check-equal? (length (pushes log)) 1)
       (check-equal? (remote-tip w) head)))
    (with-world
     (lambda (w)
       (define branch (world-branch w))
       (define head (world-head w))
       ;; A DIVERGENT remote tip (not an ancestor of the approved head).
       (git (world-work w) "checkout" "-q" "-b" "diverge" "main")
       (put (build-path (world-work w) "other.txt") "other\n")
       (git (world-work w) "add" "other.txt")
       (git (world-work w) "commit" "-m" "divergent remote commit")
       (define divergent (git (world-work w) "rev-parse" "HEAD"))
       (git (world-work w) "push" "origin"
            (string-append divergent ":refs/heads/" branch))
       (git (world-work w) "checkout" "-q" branch)
       (define log
         (with-logging-git
          w
          (lambda ()
            (check-equal? (refusal-message (lambda () (publish! w)))
                          "remote branch diverged from approved head; refusing publication"))))
       (check-equal? (pushes log) '())
       (check-equal? (remote-tip w) divergent "the refused publication mutated nothing"))))

  ;; ----------------------------------------------------------------
  ;; Readback shape: missing/ambiguous identity
  ;; ----------------------------------------------------------------
  (test-case "a readback without the expected fields refuses and never pushes"
    (with-world
     (lambda (w)
       (define log
         (build-path (world-root w) "argv.log"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "printf '%s\\n' \"$*\" >> '~a'\n"
                                "case \" $* \" in\n"
                                "  *\" ls-remote \"*) printf '%s\\t%s\\n' '~a' 'refs/heads/other'; exit 0 ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 log
                 (make-string 40 #\1)
                 real-git))
       (define bin (install-shim! (world-root w) script))
       (with-path
        bin
        (lambda ()
          (check-equal?
           (refusal-message (lambda () (publish! w)))
           (format "malformed remote readback for refs/heads/~a" (world-branch w)))))
       (define logged (logged-argv log))
       (check-equal? (pushes logged) '())
       (check-false (remote-tip w))))
    (with-world
     (lambda (w)
       (define log (build-path (world-root w) "argv.log"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "printf '%s\\n' \"$*\" >> '~a'\n"
                                "case \" $* \" in\n"
                                "  *\" ls-remote \"*) printf '%s\\t%s\\n%s\\t%s\\n' '~a' 'refs/heads/~a' '~a' 'refs/heads/~a'; exit 0 ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 log
                 (make-string 40 #\1)
                 (world-branch w)
                 (make-string 40 #\2)
                 (world-branch w)
                 real-git))
       (define bin (install-shim! (world-root w) script))
       (with-path
        bin
        (lambda ()
          (check-equal?
           (refusal-message (lambda () (publish! w)))
           (format "ambiguous remote readback for refs/heads/~a" (world-branch w)))))
       (check-equal? (pushes (logged-argv log)) '())
       (check-false (remote-tip w)))))

  ;; ----------------------------------------------------------------
  ;; Lost response: attempted ONCE; recovery is a fresh re-read
  ;; ----------------------------------------------------------------
  (test-case "a lost push response is attempted once and recovered only by a fresh re-read"
    (with-world
     (lambda (w)
       (define log (build-path (world-root w) "argv.log"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "printf '%s\\n' \"$*\" >> '~a'\n"
                                "case \" $* \" in\n"
                                "  *\" push \"*) '~a' \"$@\" || exit $?; "
                                "printf 'fatal: connection reset while awaiting response\\n' >&2; exit 128 ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 log
                 real-git
                 real-git))
       (define bin (install-shim! (world-root w) script))
       (with-path
        bin
        (lambda ()
          ;; The response is lost even though the effect landed: refusal, no retry.
          (define reason (refusal-message (lambda () (publish! w))))
          (check-true (string-contains? reason "delivery command failed") reason)
          (check-equal? (remote-tip w) (world-head w) "the single push landed")
          ;; A FRESH invocation re-reads the remote and is idempotent — it does
          ;; NOT push a second time.
          (check-equal? (publish! w)
                        (hasheq 'status "already-published"
                                'branch (world-branch w)
                                'head (world-head w)))
          (define logged (map string-split (file->lines log)))
          (check-equal? (length (pushes logged)) 1
                        "exactly one push attempt across both invocations")
          (check-equal? (remote-tip w) (world-head w)))))))

  ;; ----------------------------------------------------------------
  ;; Guard re-verification windows
  ;; ----------------------------------------------------------------
  (test-case "local-ref drift during the already-published readback refuses"
    (with-world
     (lambda (w)
       (git (world-work w) "push" "origin"
            (string-append (world-head w) ":refs/heads/" (world-branch w)))
       (define marker (build-path (world-root w) "readback-seen"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "case \" $* \" in\n"
                                "  *\" ls-remote \"*) : > '~a'; exec '~a' \"$@\" ;;\n"
                                "  *\" rev-parse refs/heads/~a \"*) "
                                "if [ -e '~a' ]; then printf '%s\\n' '~a'; exit 0; fi ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 marker
                 real-git
                 (world-branch w)
                 marker
                 (make-string 40 #\f)
                 real-git))
       (with-path
        (install-shim! (world-root w) script)
        (lambda ()
          (check-equal?
           (refusal-message (lambda () (publish! w)))
           "local branch ref drifted during the already-published readback")
          (check-equal? (remote-tip w) (world-head w)
                        "the idempotent readback mutated nothing"))))))

  (test-case "tree drift during the already-published readback refuses"
    (with-world
     (lambda (w)
       (git (world-work w) "push" "origin"
            (string-append (world-head w) ":refs/heads/" (world-branch w)))
       (define marker (build-path (world-root w) "readback-seen"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "case \" $* \" in\n"
                                "  *\" ls-remote \"*) : > '~a'; exec '~a' \"$@\" ;;\n"
                                "  *\" rev-parse ~a^{tree} \"*) "
                                "if [ -e '~a' ]; then printf '%s\\n' '~a'; exit 0; fi ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 marker
                 real-git
                 (world-head w)
                 marker
                 (make-string 40 #\e)
                 real-git))
       (with-path
        (install-shim! (world-root w) script)
        (lambda ()
          (check-equal?
           (refusal-message (lambda () (publish! w)))
           "approved tree drifted during the already-published readback")
          (check-equal? (remote-tip w) (world-head w)
                        "the idempotent readback mutated nothing"))))))

  (test-case "origin drift after the final readback refuses even though the push landed"
    (with-world
     (lambda (w)
       ;; Force the push path: the remote branch does not exist yet.
       (define branch (world-branch w))
       (define head (world-head w))
       (define count (build-path (world-root w) "config-count"))
       (define script
         (format (string-append "#!/bin/sh\n"
                                "case \" $* \" in\n"
                                "  *\" config --get remote.origin.url \"*)\n"
                                "     n=0; [ -f '~a' ] && n=$(cat '~a'); n=$((n + 1)); "
                                "printf '%s' \"$n\" > '~a'\n"
                                "     if [ \"$n\" -ge 3 ]; then printf '%s-drifted\\n' '~a'; exit 0; fi ;;\n"
                                "esac\n"
                                "exec '~a' \"$@\"\n")
                 count
                 count
                 count
                 (world-origin-url w)
                 real-git))
       (with-path
        (install-shim! (world-root w) script)
        (lambda ()
          (check-equal? (refusal-message (lambda () (publish! w)))
                        "origin drifted after the final readback")
          (check-equal? (remote-tip w) (world-head w)
                        "the push landed but was never confirmed"))))))

  ;; ----------------------------------------------------------------
  ;; CLI contract (same JSON / exit status as the Python action)
  ;; ----------------------------------------------------------------
  (test-case "CLI parity: JSON published on stdout with exit 0, typed refusal with exit 2"
    (with-world
     (lambda (w)
       (define published
         (parameterize ([current-environment-variables (fixture-env (world-root w))])
           (apply run-cli (world-root w) (cli-args w))))
       (check-equal? (subprocess-result-exit-code published) 0
                     (subprocess-result-stderr published))
       (define payload (string->jsexpr (subprocess-result-stdout published)))
       (check-equal? (hash-ref payload 'status) "published")
       (check-equal? (hash-ref payload 'branch) (world-branch w))
       (check-equal? (hash-ref payload 'head) (world-head w))
       ;; a refusal is the exact delivery-pending shape on stdout, exit 2
       (define refused
         (parameterize ([current-environment-variables (fixture-env (world-root w))])
           (apply run-cli
                  (world-root w)
                  (except-flag (cli-args w) "--expected-tree"))))
       (check-equal? (subprocess-result-exit-code refused) 2)
       (check-equal? (subprocess-result-stderr refused) "")
       (define refused-json (string->jsexpr (subprocess-result-stdout refused)))
       (check-equal? (hash-ref refused-json 'status) "delivery-pending")
       (check-equal?
        (hash-ref refused-json 'reason)
        "publish-head requires --expected-head, --expected-branch, --expected-origin and --expected-tree")))
    (with-world
     (lambda (w)
       (define refused
         (parameterize ([current-environment-variables (fixture-env (world-root w))])
           (apply run-cli
                  (world-root w)
                  (set-flag-value (cli-args w) "--expected-branch" "main"))))
       (check-equal? (subprocess-result-exit-code refused) 2)
       (check-equal? (hash-ref (string->jsexpr (subprocess-result-stdout refused)) 'reason)
                     "publish-head refuses protected branch main")
       (check-false (remote-tip w)))))

  ;; ----------------------------------------------------------------
  ;; Recorded payload shape (the deleted action's exact key set)
  ;; ----------------------------------------------------------------
  (test-case "the published payload keeps the recorded action key set"
    ;; A live differential against the Python action is no longer possible (the
    ;; Phase B final deletion removed it), so this case pins what that parity
    ;; proved: the exact JSON payload the action produced (recorded at its final
    ;; revision), the same shape from the CLI as from the library, and a
    ;; byte-identical payload for an identical second run.
    (with-world
     (lambda (w)
       (define branch (world-branch w))
       (define head (world-head w))
       (define first-run
         (parameterize ([current-environment-variables (fixture-env (world-root w))])
           (apply run-cli (world-root w) (cli-args w))))
       (check-equal? (subprocess-result-exit-code first-run)
                     0
                     (subprocess-result-stderr first-run))
       (define payload (string->jsexpr (subprocess-result-stdout first-run)))
       (check-equal? payload (hasheq 'status "published" 'branch branch 'head head))
       (check-equal? (sort (map symbol->string (hash-keys payload)) string<?)
                     '("branch" "head" "status"))
       ;; idempotent replay: still the recorded shape, and no second push
       (define second-run
         (parameterize ([current-environment-variables (fixture-env (world-root w))])
           (apply run-cli (world-root w) (cli-args w))))
       (check-equal? (subprocess-result-exit-code second-run) 0)
       (check-equal? (hash-ref (string->jsexpr (subprocess-result-stdout second-run)) 'status)
                     "already-published")
       (check-equal? (remote-tip w) head))))

  ;; ----------------------------------------------------------------
  ;; Production publication owner is native and binds the exact evidence
  ;; ----------------------------------------------------------------
  (test-case "production publisher is native: exact evidence is recorded and python3 is never spawned"
    (with-world
     (lambda (w)
       (define base (make-temporary-file "publish-base-~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (define plan (world-plan w))
          (define identity
            (hasheq 'repo (path->string (world-work w))
                    'branch (world-branch w)
                    'head (world-head w)
                    'tree (world-tree w)
                    'origin (world-origin-url w)))
          (define attempt (campaign-attempt "attempt-1" 7 0))
          (define (authorized-record)
            (define rec
              (make-campaign-record plan
                                    (make-campaign-manifest 1 "delivery" '() '() "constraints")
                                    (list (make-campaign-wave 0 "delivery" 'verifying 1 attempt))
                                    #f
                                    9
                                    'test
                                    0
                                    0))
            (set-campaign-record-plan-snapshot-digest! rec (make-string 64 #\d))
            rec)
          (define evidence "((verify #t . passed))")
          (define approved-verification
            (delivery-verification #t '((verify . (#t . "passed"))) "approved"))
          ;; A python3 spy: the native publication owner must never reach it.
          (define bin (build-path (world-root w) "bin"))
          (define marker (build-path (world-root w) "python-invoked"))
          (make-directory bin)
          (define spy (build-path bin "python3"))
          (put spy (format "#!/bin/sh\nprintf invoked > '~a'\nexit 97\n" marker))
          (file-or-directory-permissions spy #o700)
          (define head-before (git (world-work w) "rev-parse" "HEAD"))
          (define result
            (parameterize ([current-environment-variables
                            (fixture-env (world-root w)
                                         (format "~a:/usr/bin:/bin" (path->string bin)))])
              (verify-with-delivery-receipt
               base
               plan
               0
               base
               (lambda () approved-verification)
               #:approved? (lambda (v)
                             (and (delivery-verification? v)
                                  (delivery-verification-approved? v)))
               #:evidence delivery-verification-evidence
               #:attempt attempt
               #:current-record authorized-record
               #:coordinator-fence 9
               #:publish-approved-head publish-approved-head!
               #:remote-published (lambda (_repo _branch _head) #f)
               #:snapshot (lambda (_) identity))))
          (check-equal? result approved-verification)
          ;; the caller's durable evidence binding is unchanged and exact
          (define publication (load-approved-publication base plan 0))
          (check-equal? (hash-ref publication 'status) "confirmed")
          (check-equal? (hash-ref publication 'branch) (world-branch w))
          (check-equal? (hash-ref publication 'head) (world-head w))
          (check-equal? (hash-ref (hash-ref publication 'receipt-candidate) 'evidence)
                        evidence)
          (define receipt (hash-ref (load-delivery-journal base plan 0) 'receipt))
          (check-equal? (hash-ref receipt 'head) (world-head w))
          (check-equal? (hash-ref receipt 'branch) (world-branch w))
          ;; the exact approved head is published; nothing else moved
          (check-equal? (remote-tip w) (world-head w))
          (check-equal? (git (world-work w) "rev-parse" "HEAD") head-before)
          (check-equal? (git (world-work w) "status" "--porcelain") "")
          ;; publish_head writes NO local file into the repository
          (check-false (directory-exists? (build-path (world-work w) ".planning")))
          (check-false (file-exists? marker) "the publication owner must not spawn python3"))
        (lambda () (delete-directory/files base)))))))
