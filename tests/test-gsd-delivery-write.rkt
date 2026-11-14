#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-write.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 420
;; Phase B slice S5: the GitHub WRITE seam (`gh_put`, `gh_post`) and the
;; `open-pr` action of scripts/gsd-delivery.py are ported to Racket
;; (scripts/gsd-delivery-write.rkt). The module is NOT wired into the
;; coordinator by this slice, the Python controller is untouched, and the S3/S4
;; modules it reuses are read-only here.
;;
;; Parity law under test: every refusal is a typed exn:fail:delivery-action
;; carrying the EXACT Python reason string, and every positive path returns the
;; same JSON object. A fake `gh` executable is installed FIRST on PATH and logs
;; every invocation's ARGV VERBATIM (unit-separator between elements, record
;; separator after the last one), so an argument containing `?`, `&`, spaces or
;; the multi-line PR body is asserted from evidence: a shell would have split it,
;; and a retry would show up as a second POST.
;;
;; `git` is a logging FORWARDER (logs argv, then execs the real git), so the
;; combined log proves ORDER: resolve-before-create must appear BEFORE the
;; origin/main refresh, the wave-branch force-fetch and the POST. A real local
;; origin/work/subject fixture backs every git command, so refresh, the forced
;; refspec, rev-parse, merge-base and the drift diff are REAL commands: deleting
;; a fetch or a rev-parse cannot hide behind a stub.
;;
;; NON-IDEMPOTENT-POST GUARD under test: a rejected POST surfaces the seam's
;; typed, credential-free failure and the log holds EXACTLY ONE POST (no
;; automatic retry); a second action against the same repository resolves the
;; existing PR and spawns NO second POST.
;;
;; One-budget law under test: the mutation is executed by the delivery context's
;; OWN bounded runner, so a context whose budget is already spent refuses with
;; the typed `budget-exhausted` cause BEFORE spawning gh (a second, fresh runner
;; would have succeeded).
;;
;; Negative fixtures kill one guard at a time: a creation response that is a
;; list/string/broken JSON, a creation response with no number / zero / negative
;; / string / float / boolean number, a re-read that is not an object, is
;; closed, has a foreign head or base repository, a non-main base, an unusable
;; number, a short head SHA, a head that is not the fetched tip, a head ref that
;; is not the expected branch, a non-ancestor receipt head, and a receipt head
;; followed by a NON-evidence commit.
;;
;; Differential parity: every case's Python outcome (success payload or refusal
;; message) was captured from scripts/gsd-delivery.py at its final revision into
;; tests/fixtures/gsd-delivery-goldens.json before the controller was deleted
;; (Phase B), driven through the SAME gh fixtures against the SAME git subject;
;; every case declares its OWN expected outcome ('ok or the exact reason string)
;; and BOTH sides are asserted against it, so a fixture drift cannot let two
;; identical transport failures masquerade as parity. Git object names and the
;; temp subject path are tokenized in the recording and substituted back from the
;; live fixtures, and the golden loader pins the case count, the label set and
;; each case's input text, so a stale row can never answer a different case. No
;; interpreter is needed on any host. `gh_graphql` is deliberately excluded: the
;; Python controller had no GraphQL call to compare against.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-pr.rkt"
         "../scripts/gsd-delivery-write.rkt"
         "helpers/gsd-delivery-goldens.rkt")

(define-runtime-path write-module "../scripts/gsd-delivery-write.rkt")

;; Resolved at load time, BEFORE any fixture rewrites PATH, so the logging `git`
;; forwarder can exec the real binary.
(define real-git (path->string (or (find-executable-path "git") "/usr/bin/git")))
(define racket-executable (or (find-executable-path "racket") "racket"))

(define slug "coinerd/write")
(define plan-id (make-string 64 #\a))
(define w1-branch "campaign/w1")
(define w2-branch "campaign/w2")
(define drift-branch "campaign/drift")

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

;; No user/system Git config, no ambient credential helpers: PATH is explicit.
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

(define (json-text value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

(define (git dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 30
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

(define (install-shim! bin name script)
  (make-directory* bin)
  (define shim (build-path bin name))
  (put shim script)
  (file-or-directory-permissions shim #o700)
  shim)

;; ============================================================
;; Verbatim argv logging
;; ============================================================
;;
;; Each invocation is logged as: program name, then one unit-separator (0x1f)
;; before EVERY argument, then a record separator (0x1e). Newlines inside an
;; argument therefore stay inside ONE record, so the multi-line PR body is
;; compared byte-for-byte and a record can never be split by a value.

(define record-separator (string #\u001E))
(define unit-separator (string #\u001F))

(define (argv-log-snippet marker)
  (format (string-append "{ printf '%s' ~a"
                         "; for a in \"$@\"; do printf '\\037%s' \"$a\"; done"
                         "; printf '\\036'; } >> \"$WRITE_SHIM_LOG\"")
          marker))

(define (read-records log)
  (if (file-exists? log)
      (for/list ([record (in-list (string-split (file->string log) record-separator))]
                 #:unless (string=? record ""))
        (string-split record unit-separator))
      '()))

;; Invocations of `program` (default: all), without the program marker.
(define (invocations log [program #f])
  (for/list ([record (in-list (read-records log))]
             #:when (or (not program) (equal? (car record) program)))
    (cdr record)))

(define (argv-method argv)
  (define at (index-of argv "-X"))
  (and at (< (add1 at) (length argv)) (list-ref argv (add1 at))))

(define (mutations log method)
  (for/list ([argv (in-list (invocations log "gh"))]
             #:when (equal? (argv-method argv) method))
    argv))

(define (record-position log predicate)
  (for/first ([record (in-list (read-records log))]
              [index (in-naturals)]
              #:when (predicate record))
    index))

(define (record-position! log predicate what)
  (or (record-position log predicate) (fail (format "the shim log carries no ~a invocation" what))))

;; ============================================================
;; Shim scripts
;; ============================================================

;; routes: list of (route-string . fixture-file-name).
;; rejects: list of (list route-string stderr-text exit-code), served as a
;; FAILING mutation so the retry guard is observable.
(define (gh-shim-script routes rejects)
  (string-append
   "#!/bin/sh\n"
   (argv-log-snippet "gh")
   "\nroute=\n"
   "for arg in \"$@\"; do case \"$arg\" in repos/*) route=$arg ;; esac; done\n"
   "case \"$route\" in\n"
   (string-join
    (append (for/list ([entry (in-list routes)])
              (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cdr entry)))
            (for/list ([entry (in-list rejects)])
              (format "  ~s) printf '%s\\n' ~s >&2; exit ~a ;;"
                      (list-ref entry 0)
                      (list-ref entry 1)
                      (list-ref entry 2)))
            (list "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;"))
    "\n")
   "\nesac\n"))

;; A real-git forwarder that records the argv first: the combined log proves the
;; ORDER of gh reads against git reads and fetches.
(define (git-forwarder-script)
  (string-append "#!/bin/sh\n" (argv-log-snippet "git") "\n" (format "exec ~s \"$@\"\n" real-git)))

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

;; GitHub's pulls head filter is `owner:branch`, so the resolve route is keyed by
;; the OWNER plus the branch (converted into a route of this repository).
(define (resolve-route slug* branch*)
  (route slug* (format "pulls?state=open&head=coinerd:~a" branch*)))

;; ============================================================
;; Real git fixture (built ONCE, immutable for the whole run)
;; ============================================================

(define (build-repo-world root)
  (define origin (build-path root "origin.git"))
  (define work (build-path root "work"))
  (define subject (build-path root "subject"))
  (git root "init" "--bare" "-b" "main" (path->string origin))
  (git root "init" "-b" "main" (path->string work))
  (put (build-path work "tracked") "initial\n")
  (git work "add" "tracked")
  (git work "commit" "-m" "initial")
  (define main (git work "rev-parse" "HEAD"))
  (git work "remote" "add" "origin" (path->string origin))
  (git work "push" "origin" "main")
  ;; campaign/w1: one implementation commit on main.
  (git work "checkout" "-b" w1-branch main)
  (put (build-path work "feature") "impl\n")
  (git work "add" "feature")
  (git work "commit" "-m" "impl")
  (define w1-tip (git work "rev-parse" "HEAD"))
  (git work "push" "origin" w1-branch)
  ;; campaign/w2: a durable receipt head plus one excluded-evidence commit.
  (git work "checkout" "-b" w2-branch main)
  (put (build-path work "impl-two") "impl two\n")
  (git work "add" "impl-two")
  (git work "commit" "-m" "impl two")
  (define receipt (git work "rev-parse" "HEAD"))
  (put (build-path work "docs/reports/gsd-wave-evidence" (format "~a-w0.rktd" plan-id)) "{}\n")
  (git work "add" "docs/reports/gsd-wave-evidence")
  (git work "commit" "-m" "evidence")
  (define w2-tip (git work "rev-parse" "HEAD"))
  (git work "push" "origin" w2-branch)
  ;; campaign/drift: the receipt head plus a SOURCE commit.
  (git work "checkout" "-b" drift-branch receipt)
  (put (build-path work "src-drift") "drift\n")
  (git work "add" "src-drift")
  (git work "commit" "-m" "drift")
  (define drift-tip (git work "rev-parse" "HEAD"))
  (git work "push" "origin" drift-branch)
  (git root "clone" (path->string origin) (path->string subject))
  (git subject "config" "user.name" "Test")
  (git subject "config" "user.email" "test@example.invalid")
  (hasheq 'root
          root
          'origin
          origin
          'work
          work
          'subject
          subject
          'main
          main
          'w1-tip
          w1-tip
          'receipt
          receipt
          'w2-tip
          w2-tip
          'drift-tip
          drift-tip))

;; The subject is only READ and fetched from (tracking refs may move), so the
;; repository is built once; every case still gets its OWN shim directory and
;; log, so every spawn is attributable per case.
(define repo-world-box (box #f))

(define (shared-repo-world)
  (or (unbox repo-world-box)
      (let* ([root (make-temporary-file "native-write-repo-~a" 'directory)]
             [env (fixture-env root "/usr/bin:/bin")])
        (define world
          (parameterize ([current-environment-variables env])
            (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
            (build-repo-world root)))
        (set-box! repo-world-box world)
        world)))

;; Point the subject's CONFIGURED origin URL at a GitHub slug while the
;; insteadOf rewrite keeps every git transport local. That distinction is
;; exactly what repository() must respect.
(define (point-at! world slug*)
  (define subject (hash-ref world 'subject))
  (define origin (hash-ref world 'origin))
  (define url (format "https://github.com/~a" slug*))
  (git subject "config" "remote.origin.url" url)
  (git subject "config" (format "url.~a.insteadOf" (path->string origin)) url))

;; ============================================================
;; Per-case world: shared git fixture + fresh shims and log
;; ============================================================

(define (install-world-shims! root fixtures rejects)
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
  ;; route -> fixture index, so one case can swap the payload a route serves.
  (put (build-path root "routes.tsv")
       (string-join (for/list ([entry (in-list routes)]
                               [index (in-naturals)])
                      (format "~a\t~a" index (car entry)))
                    "\n"))
  (install-shim! bin "gh" (gh-shim-script routes rejects))
  (install-shim! bin "git" (git-forwarder-script))
  (define env (fixture-env root (format "~a:/usr/bin:/bin" bin)))
  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
  (environment-set! env "WRITE_SHIM_LOG" (path->string (build-path root "shim.log")))
  env)

;; Rewrite the fixture a route serves, IN PLACE, so a case can model the service
;; state changing between two actions without reinstalling the shim.
(define (set-fixture! root route-text body)
  (define line
    (for/first ([entry (in-list (file->lines (build-path root "routes.tsv")))]
                #:when (equal? (cadr (string-split entry "\t")) route-text))
      entry))
  (unless line
    (error 'set-fixture! "no installed fixture for route ~a" route-text))
  (define index (car (string-split line "\t")))
  (put (build-path root "fixtures" (format "fixture-~a.json" index)) body))

;; fixtures is a list of (route . body) OR a procedure of the built world.
;; proc receives (world root log).
(define (with-write-world fixtures slug* proc #:rejects [rejects '()])
  (define world (shared-repo-world))
  (define root (make-temporary-file "native-write-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (when slug*
                    (point-at! world slug*))
                  (define env
                    (install-world-shims! root
                                          (if (procedure? fixtures)
                                              (fixtures world)
                                              fixtures)
                                          rejects))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "gh")
                                     "the fixture must resolve gh through PATH")
                    (check-equal? (path->complete-path (find-executable-path "git"))
                                  (path->complete-path (build-path root "bin" "git"))
                                  "the fixture must resolve git to the logging forwarder")
                    (clear-api-cache!)
                    (proc world root (build-path root "shim.log"))))
                (lambda () (delete-directory/files root))))

;; ============================================================
;; JSON fixture constructors
;; ============================================================

(define (pr-json #:number [number 1]
                 #:state [state "open"]
                 #:head-sha [head-sha (make-string 40 #\d)]
                 #:head-ref [head-ref w1-branch]
                 #:base-ref [base-ref "main"]
                 #:base-repo [base-repo slug]
                 #:head-repo [head-repo slug])
  (hasheq 'number
          number
          'state
          state
          'head
          (hasheq 'ref head-ref 'sha head-sha 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref base-ref 'sha (make-string 40 #\c) 'repo (hasheq 'full_name base-repo))))

;; The exact multi-line body Python's open_pr sends (independent literal, so a
;; module-side edit cannot make the assertion agree with the module).
(define (expected-body plan wave tip branch* evidence)
  (string-append "Plan: "
                 plan
                 "\nWave: W"
                 (number->string wave)
                 "\nHead: "
                 tip
                 "\nBranch: "
                 branch*
                 "\nBinding evidence: "
                 evidence
                 "\nReview: genuine independent review required at the exact implementation head.\n"))

(define (evidence-path plan wave)
  (format "docs/reports/gsd-wave-evidence/~a-w~a.rktd" plan wave))

;; The full happy-path fixture set for ONE slug/branch.
(define (create-fixtures slug* world branch* tip)
  (list
   (cons (resolve-route slug* branch*) "[]")
   (cons (route slug* "pulls") "{\"number\":1}")
   (cons
    (route slug* "pulls/1")
    (json-text
     (pr-json #:number 1 #:head-sha tip #:head-ref branch* #:base-repo slug* #:head-repo slug*)))))

;; ============================================================
;; Refusal / canonicalization helpers
;; ============================================================

(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

(define (canonical value)
  (cond
    [(void? value) 'null]
    [(eq? value 'null) 'null]
    [(hash? value)
     (cons 'object
           (for/list ([key (in-list (sort (map symbol->string (hash-keys value)) string<?))])
             (cons key (canonical (hash-ref value (string->symbol key))))))]
    [(list? value) (cons 'array (map canonical value))]
    [else value]))

(define (json-value value)
  (cond
    [(void? value) 'null]
    [(eq? value #f) 'null]
    [else value]))

;; ============================================================
;; The recorded oracle answers
;; ============================================================
(define parity-group "gsd-delivery-write")

(define (parity-case slug* fixtures expected thunk spec [rejects '()])
  (hasheq 'slug slug* 'fixtures fixtures 'expected expected 'thunk thunk 'spec spec 'rejects rejects))

(define rejected-message
  "delivery command failed (gh, exit 74); inspect authentication, repository refs and required artifacts locally")

(module+ test
  ;; ============================================================
  ;; the write seam: exact argv, parsing, one attempt, one budget
  ;; ============================================================

  (test-case "mutation-argv reproduces Python's `sorted(fields)` order"
    (check-equal? (mutation-argv
                   "POST"
                   "o/r"
                   "pulls"
                   (list (cons "title" "T") (cons "head" "b") (cons "base" "main") (cons "body" "B")))
                  (list "api"
                        "--hostname"
                        "github.com"
                        "-X"
                        "POST"
                        "repos/o/r/pulls"
                        "-f"
                        "base=main"
                        "-f"
                        "body=B"
                        "-f"
                        "head=b"
                        "-f"
                        "title=T"))
    (check-equal? (delivery-refusal (lambda () (mutation-argv "PUT" "o/r" "x" (list (cons "" "v")))))
                  "invalid GitHub mutation field")
    (check-equal? (delivery-refusal (lambda () (mutation-argv "PUT" "o/r" "x" "not-fields")))
                  "invalid GitHub mutation fields"))

  (test-case "gh-put runs the exact Python argv through the context's runner"
    (with-write-world
     (list (cons (route slug "pulls/7/merge") "{\"merged\":true}"))
     #f
     (lambda (world root log)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       ;; Fields arrive UNSORTED: the seam must order them like Python.
       (check-equal?
        (gh-put ctx slug "pulls/7/merge" (list (cons "z" "1") (cons "merge_method" "squash")))
        (hasheq 'merged #t))
       (check-equal? (car (invocations log "gh"))
                     (list "api"
                           "--hostname"
                           "github.com"
                           "-X"
                           "PUT"
                           (route slug "pulls/7/merge")
                           "-f"
                           "merge_method=squash"
                           "-f"
                           "z=1")))))

  (test-case "a non-object mutation response is the exact Python refusal"
    (for ([payload (in-list (list "[]" "\"text\"" "12" "null" "not-json"))])
      (with-write-world (list (cons (route slug "pulls") payload))
                        #f
                        (lambda (world root log)
                          (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
                          (check-equal? (delivery-refusal (lambda () (gh-post ctx slug "pulls" '())))
                                        "malformed GitHub mutation response"
                                        payload)))))

  (test-case "a rejected POST surfaces the typed failure after EXACTLY one attempt"
    (with-write-world
     '()
     #f
     #:rejects (list (list (route slug "pulls") "Validation Failed" 74))
     (lambda (world root log)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (check-equal? (delivery-refusal (lambda ()
                                         (gh-post ctx slug "pulls" (list (cons "title" "T")))))
                     rejected-message)
       (check-equal? (length (mutations log "POST"))
                     1
                     "a POST is non-idempotent: it must never be retried"))))

  (test-case "the mutation spends the session's ONE budget (a spent budget refuses before spawning)"
    (with-write-world
     '()
     #f
     (lambda (world root log)
       (define ctx
         (make-delivery-pr-context (hash-ref world 'subject)
                                   #:budget 1.0
                                   #:start-ms (current-inexact-monotonic-milliseconds)))
       (define reason (delivery-refusal (lambda () (gh-post ctx slug "pulls" '()))))
       (check-true (string-prefix? reason "budget-exhausted:") reason)
       (check-equal? (invocations log) '() "a spent budget must refuse before spawning gh"))))

  (test-case "gh-graphql keeps the caller's query in ONE argv element"
    (with-write-world
     (list (cons "" "{\"data\":{\"ok\":true}}"))
     #f
     (lambda (world root log)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (check-equal? (gh-graphql ctx "query { viewer { login } }" (list (cons "n" "1")))
                     (hasheq 'data (hasheq 'ok #t)))
       (check-equal? (car (invocations log "gh"))
                     (list "api"
                           "--hostname"
                           "github.com"
                           "graphql"
                           "-X"
                           "POST"
                           "-f"
                           "query=query { viewer { login } }"
                           "-f"
                           "n=1"))
       (check-equal? (delivery-refusal (lambda () (gh-graphql ctx "   " '())))
                     "graphql requires a non-empty query"))))

  (test-case "gh-graphql refuses a non-object response"
    (with-write-world
     (list (cons "" "[]"))
     #f
     (lambda (world root log)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (check-equal? (delivery-refusal (lambda () (gh-graphql ctx "{ viewer { login } }" '())))
                     "malformed GitHub mutation response"))))

  ;; ============================================================
  ;; open-pr: creation, reuse, ordering
  ;; ============================================================

  (test-case "open-pr creates the PR once with the exact argv, title, and body"
    (with-write-world
     (lambda (world) (create-fixtures slug world w1-branch (hash-ref world 'w1-tip)))
     slug
     (lambda (world root log)
       (define tip (hash-ref world 'w1-tip))
       (define evidence (evidence-path plan-id 1))
       (check-equal? (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)
                     (hasheq 'status "opened" 'pr 1 'branch w1-branch 'head tip))
       (define posts (mutations log "POST"))
       (check-equal? (length posts) 1)
       (check-equal? (car posts)
                     (list "api"
                           "--hostname"
                           "github.com"
                           "-X"
                           "POST"
                           (route slug "pulls")
                           "-f"
                           "base=main"
                           "-f"
                           (string-append "body=" (expected-body plan-id 1 tip w1-branch evidence))
                           "-f"
                           "head=campaign/w1"
                           "-f"
                           "title=W1 delivery: campaign/w1"))
       ;; The created identity is re-read from GitHub before it is returned.
       (check-true (for/or ([argv (in-list (invocations log "gh"))])
                     (equal? argv (list "api" "--hostname" "github.com" (route slug "pulls/1"))))))))

  (test-case "open-pr accepts an evidence-only descendant of the receipt head"
    (with-write-world
     (lambda (world) (create-fixtures slug world w2-branch (hash-ref world 'w2-tip)))
     slug
     (lambda (world root log)
       (check-equal? (delivery-open-pr! (hash-ref world 'subject)
                                        plan-id
                                        1
                                        w2-branch
                                        #:expected-head (hash-ref world 'receipt))
                     (hasheq 'status "opened" 'pr 1 'branch w2-branch 'head (hash-ref world 'w2-tip)))
       (check-equal? (length (mutations log "POST")) 1))))

  (test-case "a second open-pr reuses the existing PR and spawns no POST"
    (with-write-world
     (lambda (world) (create-fixtures slug world w1-branch (hash-ref world 'w1-tip)))
     slug
     (lambda (world root log)
       (define subject (hash-ref world 'subject))
       (define tip (hash-ref world 'w1-tip))
       (check-equal? (hash-ref (delivery-open-pr! subject plan-id 1 w1-branch) 'status) "opened")
       ;; The service now reports the PR the first call created.
       (set-fixture! root
                     (resolve-route slug w1-branch)
                     (json-text (list (pr-json #:number 1 #:head-sha tip))))
       (check-equal? (delivery-open-pr! subject plan-id 1 w1-branch)
                     (hasheq 'status "opened" 'pr 1 'branch w1-branch 'head tip))
       (check-equal? (length (mutations log "POST"))
                     1
                     "the reuse path must never create a second PR"))))

  (test-case "resolve-before-create happens BEFORE the refresh, the fetch and the POST"
    (with-write-world
     (lambda (world) (create-fixtures slug world w1-branch (hash-ref world 'w1-tip)))
     slug
     (lambda (world root log)
       (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)
       (define resolve-at
         (record-position! log
                           (lambda (record)
                             (and (equal? (car record) "gh")
                                  (member (resolve-route slug w1-branch) record)))
                           "existing-PR resolve"))
       (define refresh-at
         (record-position! log
                           (lambda (record)
                             (and (equal? (car record) "git")
                                  (member "refs/heads/main:refs/remotes/origin/main" record)))
                           "origin/main refresh"))
       (define fetch-at
         (record-position!
          log
          (lambda (record)
            (and (equal? (car record) "git")
                 (member (format "+refs/heads/~a:refs/remotes/origin/~a" w1-branch w1-branch)
                         record)))
          "wave-branch fetch"))
       (define post-at
         (record-position! log
                           (lambda (record)
                             (and (equal? (car record) "gh")
                                  (equal? (argv-method (cdr record)) "POST")))
                           "POST"))
       (check-true (< resolve-at refresh-at))
       (check-true (< refresh-at fetch-at))
       (check-true (< fetch-at post-at)))))

  (test-case "open-pr refuses a non-ancestor receipt head and non-evidence drift before any POST"
    (with-write-world
     (list (cons (resolve-route slug w1-branch) "[]") (cons (resolve-route slug drift-branch) "[]"))
     slug
     (lambda (world root log)
       (define subject (hash-ref world 'subject))
       (define w2-tip (hash-ref world 'w2-tip))
       (define receipt (hash-ref world 'receipt))
       (check-equal?
        (delivery-refusal (lambda ()
                            (delivery-open-pr! subject plan-id 1 w1-branch #:expected-head w2-tip)))
        (format (string-append "receipt head ~a is not an ancestor of tip ~a; "
                               "the wave branch may have been rewritten; re-verify before delivery")
                (substring w2-tip 0 12)
                (substring (hash-ref world 'w1-tip) 0 12)))
       (check-equal?
        (delivery-refusal
         (lambda () (delivery-open-pr! subject plan-id 1 drift-branch #:expected-head receipt)))
        (format (string-append "receipt head ~a is verified but later wave-branch commits touch "
                               "non-evidence paths: src-drift; re-verify before delivery")
                (substring receipt 0 12)))
       (check-equal? (mutations log "POST") '() "a refused receipt tip must not create a PR"))))

  (test-case "open-pr validates the campaign plan/wave BEFORE mutating anything"
    (with-write-world
     (list (cons (resolve-route slug w1-branch) "[]"))
     slug
     (lambda (world root log)
       (define subject (hash-ref world 'subject))
       (for ([plan (in-list (list #f "short" (make-string 63 #\a)))])
         (check-equal? (delivery-refusal (lambda () (delivery-open-pr! subject plan 1 w1-branch)))
                       "invalid campaign id"
                       (format "~s" plan)))
       (for ([wave (in-list (list #f -1 "1" 1.0))])
         (check-equal? (delivery-refusal (lambda ()
                                           (delivery-open-pr! subject plan-id wave w1-branch)))
                       "invalid wave index"
                       (format "~s" wave)))
       (check-equal? (mutations log "POST") '())))))

;; ============================================================
;; open-pr: creation-response refusals
;; ============================================================

(test-case "open-pr refuses a creation response without a usable number"
  (for ([payload (in-list (list "{}"
                                "{\"number\":0}"
                                "{\"number\":-3}"
                                "{\"number\":\"1\"}"
                                "{\"number\":1.5}"
                                "{\"number\":true}"))])
    (with-write-world
     (list (cons (resolve-route slug w1-branch) "[]") (cons (route slug "pulls") payload))
     slug
     (lambda (world root log)
       (check-equal? (delivery-refusal
                      (lambda () (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)))
                     "GitHub pull-request creation response has no usable number"
                     payload)
       (check-equal? (length (mutations log "POST")) 1 payload)))))

(test-case "open-pr surfaces a malformed creation response from the seam"
  ;; gh_post only ever returns an object, so the reachable refusal for a
  ;; non-object creation payload is the seam's "malformed GitHub mutation
  ;; response"; open-pr's own "malformed GitHub pull-request creation
  ;; response" guard mirrors Python's defensive require and is unreachable
  ;; through this seam.
  (for ([payload (in-list (list "[]" "\"created\"" "not-json"))])
    (with-write-world
     (list (cons (resolve-route slug w1-branch) "[]") (cons (route slug "pulls") payload))
     slug
     (lambda (world root log)
       (check-equal? (delivery-refusal
                      (lambda () (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)))
                     "malformed GitHub mutation response"
                     payload)))))

;; ============================================================
;; open-pr: re-read validation
;; ============================================================

(test-case "open-pr validates the re-read PR identity it returns"
  (define (variants tip)
    (list (list "malformed PR payload" "[]" "malformed pull-request response")
          (list "closed PR"
                (json-text (pr-json #:number 2 #:state "closed" #:head-sha tip))
                "pull request is not open")
          (list "foreign head repository"
                (json-text (pr-json #:number 2 #:head-sha tip #:head-repo "fork/q"))
                "implementation PR must be a same-repository main-base pull request")
          (list "foreign base repository"
                (json-text (pr-json #:number 2 #:head-sha tip #:base-repo "other/q"))
                "implementation PR must be a same-repository main-base pull request")
          (list "non-main base"
                (json-text (pr-json #:number 2 #:head-sha tip #:base-ref "develop"))
                "implementation PR must be a same-repository main-base pull request")
          (list "unusable number"
                (json-text (pr-json #:number 0 #:head-sha tip))
                "created pull request has no usable number")
          (list "short head SHA"
                (json-text (pr-json #:number 2 #:head-sha "deadbeef"))
                "created pull request has no full head SHA")
          (list "head is not the fetched tip"
                (json-text (pr-json #:number 2 #:head-sha (make-string 40 #\e)))
                "fetched implementation branch tip differs from pull-request head")
          (list "wrong branch"
                (json-text (pr-json #:number 2 #:head-sha tip #:head-ref "campaign/other"))
                "pull request branch does not match expected branch")
          (list "wrong owner in returned head ref"
                (json-text (pr-json #:number 2 #:head-sha tip #:head-ref w2-branch))
                "pull request branch does not match expected branch")))
  (with-write-world
   (lambda (world)
     (define tip (hash-ref world 'w1-tip))
     ;; Every variant serves the SAME successful creation, then a broken read
     ;; that the loop installs into the pulls/2 slot.
     (list (cons (resolve-route slug w1-branch) "[]")
           (cons (route slug "pulls") "{\"number\":2}")
           (cons (route slug "pulls/2") "{}")))
   slug
   (lambda (world root log)
     (define subject (hash-ref world 'subject))
     (for ([variant (in-list (variants (hash-ref world 'w1-tip)))])
       (define label (list-ref variant 0))
       (define payload (list-ref variant 1))
       (define expected (list-ref variant 2))
       (set-fixture! root (route slug "pulls/2") payload)
       (check-equal? (delivery-refusal (lambda () (delivery-open-pr! subject plan-id 1 w1-branch)))
                     expected
                     label)))))

(test-case "open-pr refuses an existing PR with no usable number"
  (with-write-world
   (list (cons (resolve-route slug w1-branch) (json-text (list (pr-json #:number 0)))))
   slug
   (lambda (world root log)
     (check-equal?
      (delivery-refusal (lambda () (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)))
      "resolved pull request has no usable number")
     (check-equal? (mutations log "POST") '()))))

(test-case "open-pr refuses two open PRs for the durable branch"
  (with-write-world
   (list (cons (resolve-route slug w1-branch)
               (json-text (list (pr-json #:number 1) (pr-json #:number 2)))))
   slug
   (lambda (world root log)
     (check-equal?
      (delivery-refusal (lambda () (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch)))
      (string-append "multiple pull requests already target branch " w1-branch))
     (check-equal? (mutations log "POST") '()))))

(test-case "open-pr re-reads the PR with a cache BYPASS"
  (with-write-world
   (lambda (world)
     (define tip (hash-ref world 'w1-tip))
     (list
      (cons (resolve-route slug w1-branch) (json-text (list (pr-json #:number 1 #:head-sha tip))))
      (cons (route slug "pulls/1") (json-text (pr-json #:number 1 #:state "closed" #:head-sha tip)))))
   slug
   (lambda (world root log)
     (define subject (hash-ref world 'subject))
     (define tip (hash-ref world 'w1-tip))
     (define ctx (make-delivery-pr-context subject))
     ;; Prime the session memo with the STALE (closed) payload.
     (check-equal? (hash-ref ((delivery-pr-context-api ctx) slug "pulls/1") 'state) "closed")
     ;; The service now reports the open PR; a memoized read would still see
     ;; the closed payload and refuse.
     (set-fixture! root (route slug "pulls/1") (json-text (pr-json #:number 1 #:head-sha tip)))
     (check-equal? (delivery-open-pr! subject plan-id 1 w1-branch #:context ctx)
                   (hasheq 'status "opened" 'pr 1 'branch w1-branch 'head tip))
     (check-equal? (length (mutations log "POST")) 0 "an existing PR must never be recreated"))))

;; ============================================================
;; open-pr: input refusals and the CLI surface
;; ============================================================

(test-case "open-pr requires a non-blank durable expected branch"
  (for ([branch* (in-list (list #f "" "   " 42))])
    (check-equal?
     (delivery-refusal (lambda () (delivery-open-pr! "/nonexistent-write-fixture" plan-id 1 branch*)))
     "open-pr requires --expected-branch"
     (format "~s" branch*))))

(test-case "the write CLI refuses unsupported actions and missing flags"
  (check-equal? (delivery-refusal (lambda () (delivery-write-action! '("merge" "--repo" "/tmp/x"))))
                "unsupported action merge; this module implements open-pr only")
  (check-equal? (delivery-refusal (lambda () (delivery-write-action! '("open-pr"))))
                "open-pr requires --repo")
  (check-equal? (delivery-refusal (lambda () (delivery-write-action! '("open-pr" "--repo" "/tmp/x"))))
                "open-pr requires --expected-branch")
  (check-equal? (delivery-refusal
                 (lambda ()
                   (delivery-write-action!
                    '("open-pr" "--repo" "/tmp/x" "--wave" "one" "--expected-branch" "campaign/w1"))))
                "invalid integer for --wave: one"))

(test-case "the write CLI prints the typed delivery-pending JSON and exits 2"
  (define result
    (run-subprocess (path->string racket-executable)
                    #:args (list (path->string write-module) "merge" "--repo" "/tmp/x")
                    #:timeout 120
                    #:process-group? #t))
  (check-equal? (subprocess-result-exit-code result) 2)
  (define payload (string->jsexpr (subprocess-result-stdout result)))
  (check-equal? (hash-ref payload 'status) "delivery-pending")
  (check-equal? (hash-ref payload 'reason)
                "unsupported action merge; this module implements open-pr only"))

;; ============================================================
;; differential parity with the imported Python controller
;; ============================================================

(define parity-cases
  (list
   (parity-case "coinerd/s5-1"
                (lambda (world)
                  (create-fixtures "coinerd/s5-1" world w1-branch (hash-ref world 'w1-tip)))
                'ok
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-2"
                (lambda (world)
                  (define tip (hash-ref world 'w1-tip))
                  (list (cons (resolve-route "coinerd/s5-2" w1-branch)
                              (json-text (list (pr-json #:number 1
                                                        #:head-sha tip
                                                        #:base-repo "coinerd/s5-2"
                                                        #:head-repo "coinerd/s5-2"))))
                        (cons (route "coinerd/s5-2" "pulls/1")
                              (json-text (pr-json #:number 1
                                                  #:head-sha tip
                                                  #:base-repo "coinerd/s5-2"
                                                  #:head-repo "coinerd/s5-2")))))
                'ok
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-3"
                (lambda (world) (list (cons (resolve-route "coinerd/s5-3" w1-branch) "[]")))
                rejected-message
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch)
                (list (list (route "coinerd/s5-3" "pulls") "Validation Failed" 74)))
   (parity-case "coinerd/s5-4"
                (list (cons (resolve-route "coinerd/s5-4" w1-branch) "[]")
                      (cons (route "coinerd/s5-4" "pulls") "[]"))
                "malformed GitHub mutation response"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-5"
                (list (cons (resolve-route "coinerd/s5-5" w1-branch) "[]")
                      (cons (route "coinerd/s5-5" "pulls") "{}"))
                "GitHub pull-request creation response has no usable number"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-6"
                (lambda (world)
                  (define tip (hash-ref world 'w1-tip))
                  (list (cons (resolve-route "coinerd/s5-6" w1-branch) "[]")
                        (cons (route "coinerd/s5-6" "pulls") "{\"number\":2}")
                        (cons (route "coinerd/s5-6" "pulls/2")
                              (json-text (pr-json #:number 2
                                                  #:state "closed"
                                                  #:head-sha tip
                                                  #:base-repo "coinerd/s5-6"
                                                  #:head-repo "coinerd/s5-6")))))
                "pull request is not open"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-7"
                (lambda (world)
                  (list (cons (resolve-route "coinerd/s5-7" w1-branch) "[]")
                        (cons (route "coinerd/s5-7" "pulls") "{\"number\":2}")
                        (cons (route "coinerd/s5-7" "pulls/2")
                              (json-text (pr-json #:number 2
                                                  #:head-sha (make-string 40 #\e)
                                                  #:base-repo "coinerd/s5-7"
                                                  #:head-repo "coinerd/s5-7")))))
                "fetched implementation branch tip differs from pull-request head"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case "coinerd/s5-8"
                (lambda (world)
                  (list (cons (resolve-route "coinerd/s5-8" w1-branch) "[]")
                        (cons (route "coinerd/s5-8" "pulls") "{\"number\":2}")
                        (cons (route "coinerd/s5-8" "pulls/2")
                              (json-text (pr-json #:number 2
                                                  #:head-sha (hash-ref world 'w1-tip)
                                                  #:head-ref "campaign/other"
                                                  #:base-repo "coinerd/s5-8"
                                                  #:head-repo "coinerd/s5-8")))))
                "pull request branch does not match expected branch"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 w1-branch))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch w1-branch))
   (parity-case
    "coinerd/s5-9"
    (list (cons (resolve-route "coinerd/s5-9" w1-branch) "[]")
          (cons (resolve-route "coinerd/s5-9" drift-branch) "[]"))
    (string-append "receipt head PLACEHOLDER is verified but later wave-branch commits touch "
                   "non-evidence paths: src-drift; re-verify before delivery")
    (lambda (world)
      (delivery-open-pr! (hash-ref world 'subject)
                         plan-id
                         1
                         drift-branch
                         #:expected-head (hash-ref world 'receipt)))
    (hasheq 'fn
            "open_pr"
            'plan
            plan-id
            'wave
            1
            'branch
            drift-branch
            'expected_head
            (hash-ref (shared-repo-world) 'receipt)))
   (parity-case "coinerd/s5-10"
                (list)
                "open-pr requires --expected-branch"
                (lambda (world) (delivery-open-pr! (hash-ref world 'subject) plan-id 1 "  "))
                (hasheq 'fn "open_pr" 'plan plan-id 'wave 1 'branch "  "))
   (parity-case "coinerd/s5-11"
                (list (cons (route "coinerd/s5-11" "pulls") "{\"number\":7}"))
                'ok
                (lambda (world)
                  (gh-post (make-delivery-pr-context (hash-ref world 'subject))
                           "coinerd/s5-11"
                           "pulls"
                           (list (cons "title" "T") (cons "base" "main"))))
                (hasheq 'fn
                        "gh_post"
                        'slug
                        "coinerd/s5-11"
                        'route
                        "pulls"
                        'fields
                        (hasheq 'title "T" 'base "main")))
   (parity-case "coinerd/s5-12"
                (list (cons (route "coinerd/s5-12" "pulls/7/merge") "[]"))
                "malformed GitHub mutation response"
                (lambda (world)
                  (gh-put (make-delivery-pr-context (hash-ref world 'subject))
                          "coinerd/s5-12"
                          "pulls/7/merge"
                          (list (cons "merge_method" "squash"))))
                (hasheq 'fn
                        "gh_put"
                        'slug
                        "coinerd/s5-12"
                        'route
                        "pulls/7/merge"
                        'fields
                        (hasheq 'merge_method "squash")))
   (parity-case "coinerd/s5-13"
                (list (cons (route "coinerd/s5-13" "pulls/7/merge") "{\"merged\":true}"))
                'ok
                (lambda (world)
                  (gh-put (make-delivery-pr-context (hash-ref world 'subject))
                          "coinerd/s5-13"
                          "pulls/7/merge"
                          (list (cons "merge_method" "squash"))))
                (hasheq 'fn
                        "gh_put"
                        'slug
                        "coinerd/s5-13"
                        'route
                        "pulls/7/merge"
                        'fields
                        (hasheq 'merge_method "squash")))))

(test-case "differential parity with the recorded Python answers (>= 8 cases)"
  ;; Every case's Python outcome was recorded from scripts/gsd-delivery.py at its
  ;; final revision (tests/fixtures/gsd-delivery-goldens.json); the golden group
  ;; pins the case count, the label set and each case's input text.
  ;; The live value of every dynamic fixture placeholder the recorded answers
  ;; were captured with (git object names and the temp subject path):
  ;; {{name}} is the full value, {{name-12}} its 12-hex prefix.
  (define parity-tokens
    (list (cons "repo" (path->string (hash-ref (shared-repo-world) 'subject)))
          (cons "w1-tip" (hash-ref (shared-repo-world) 'w1-tip))
          (cons "w2-tip" (hash-ref (shared-repo-world) 'w2-tip))
          (cons "receipt" (hash-ref (shared-repo-world) 'receipt))
          (cons "drift-tip" (hash-ref (shared-repo-world) 'drift-tip))))
  (check-true (>= (length parity-cases) 8) "the parity table must carry at least 8 cases")
  ;; ...and the recorded table must carry EXACTLY these labels.
  (check-equal? (sort (map (lambda (case) (hash-ref case 'slug)) parity-cases) string<?)
                (golden-labels parity-group)
                "the golden table must match the parity table exactly")
  (define (check-declared-outcome! label side outcome expected)
    (cond
      [(eq? expected 'ok)
       (check-true (hash-ref outcome 'ok #f)
                   (format "~a: ~a must succeed, got ~s" label side outcome))]
      [else
       (check-false (hash-ref outcome 'ok #t)
                    (format "~a: ~a must refuse, got ~s" label side outcome))
       (check-equal? (hash-ref outcome 'reason #f) expected (format "~a: ~a reason" label side))]))
  (for ([case (in-list parity-cases)])
    (define label (hash-ref case 'slug))
    (with-write-world
     (hash-ref case 'fixtures)
     label
     #:rejects (hash-ref case 'rejects)
     (lambda (world root log)
       (define subject (hash-ref world 'subject))
       (define expected (hash-ref case 'expected))
       ;; The trivially-substituted ancestry reason is built from the shared
       ;; world's receipt head, so the declaration stays exact.
       (define declared
         (if (and (string? expected) (string-contains? expected "PLACEHOLDER"))
             (string-replace expected "PLACEHOLDER" (substring (hash-ref world 'receipt) 0 12))
             expected))
       (define spec (hash-set (hash-ref case 'spec) 'repo (path->string subject)))
       (define case-fixtures
         (let ([fixtures (hash-ref case 'fixtures)])
           (if (procedure? fixtures) (fixtures world) fixtures)))
       (define oracle-outcome
         (golden-outcome parity-group
                         parity-tokens
                         label
                         (hasheq 'spec spec 'fixtures case-fixtures)))
       (define racket-outcome
         (with-handlers ([exn:fail:delivery-action? (lambda (error)
                                                      (hasheq 'ok #f 'reason (exn-message error)))])
           (hasheq 'ok #t 'value (json-value ((hash-ref case 'thunk) world)))))
       (check-declared-outcome! label "racket" racket-outcome declared)
       (check-declared-outcome! label "recorded" oracle-outcome declared)
       (check-equal? (canonical racket-outcome) (canonical oracle-outcome) label)))))
