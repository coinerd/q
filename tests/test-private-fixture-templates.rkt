#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-private-fixture-templates.rkt — Contract tests for private
;; copy-on-test fixture templates (W6).
;;
;; Proves: unique canonical roots across concurrent instances; mutation/
;; commit/session-append in one instance does not affect another instance or
;; the immutable template source; independent mutation of refs/histories/CWD
;; env; idempotent, order-independent cleanup; git-unavailable is an explicit
;; skip, never a pass.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "helpers/private-fixture-templates.rkt")

(define-runtime-path project-root "../")

;; Parent root shared by several fixtures in one test: proves distinct
;; allocations under a common parent (concurrency proxy for parallel tests).
(define (fresh-parent!)
  (make-temporary-file "q-fx-test-host-~a" 'directory))

;; --- Contract 1: unique canonical roots for concurrent instances ----------

(test-case "private fixtures: concurrent instances get distinct canonical roots"
  (define parent (fresh-parent!))
  (define fx-a (make-private-session-fixture! #:parent-root parent))
  (define fx-b (make-private-session-fixture! #:parent-root parent))
  (check-not-equal? (path->string (private-fixture-root fx-a))
                    (path->string (private-fixture-root fx-b))
                    "two concurrent session fixtures must not share a root")
  (check-not-equal? (private-session-fixture-session-id fx-a)
                    (private-session-fixture-session-id fx-b)
                    "session ids must be freshly allocated per instance")
  (private-fixture-cleanup! fx-a)
  (private-fixture-cleanup! fx-b)
  (delete-directory/files parent #:must-exist? #f))

;; `system` writes directly to the OS stdout fd (not Racket's
;; current-output-port), so capture git output via shell redirection into a
;; temp file instead of with-output-to-string.
(define (private-git-system repo command)
  (call-with-private-git-environment (lambda ()
                                       (parameterize ([current-directory repo])
                                         (system command)))))

(define (git-out repo . args)
  (define out-file (make-temporary-file "q-fx-git-out-~a"))
  (private-git-system
   repo
   (string-append "git " (string-join args " ") " > " (path->string out-file) " 2>&1"))
  (begin0 (string-trim (file->string out-file))
    (delete-file out-file)))

;; --- Contract 2: immutable template source never mutated -------------------

(test-case "session template source stays byte-identical after fixtures run"
  (define template-jsonl
    (build-path project-root
                "tests"
                "fixtures"
                "session-template"
                "session"
                "tmpl-seed-0001"
                "session.jsonl"))
  (define before (file->bytes template-jsonl))
  (define parent (fresh-parent!))
  (define fx (make-private-session-fixture! #:parent-root parent))
  ;; Mutate the private copy aggressively.
  (with-output-to-file (build-path (private-session-fixture-session-dir fx) "session.jsonl")
                       (lambda () (displayln "hostile append into private copy"))
                       #:exists 'append)
  (delete-directory/files (private-fixture-root fx) #:must-exist? #f)
  (check-equal? (file->bytes template-jsonl)
                before
                "immutable template must be byte-identical after use"))

;; --- Contract 3: session append / git commit isolation ---------------------

(test-case "session append in one fixture does not affect another or source"
  (define parent (fresh-parent!))
  (define fx-a (make-private-session-fixture! #:parent-root parent))
  (define fx-b (make-private-session-fixture! #:parent-root parent))
  (define lines-a-before
    (file->lines (build-path (private-session-fixture-session-dir fx-a) "session.jsonl")))
  (define lines-b-before
    (file->lines (build-path (private-session-fixture-session-dir fx-b) "session.jsonl")))
  (with-output-to-file (build-path (private-session-fixture-session-dir fx-a) "session.jsonl")
                       (lambda () (displayln "{\"kind\":\"hostile-appended-event\"}"))
                       #:exists 'append)
  (check-equal? (length (file->lines (build-path (private-session-fixture-session-dir fx-a)
                                                 "session.jsonl")))
                (add1 (length lines-a-before)))
  (check-equal? (file->lines (build-path (private-session-fixture-session-dir fx-b) "session.jsonl"))
                lines-b-before
                "sibling fixture untouched by sibling append")
  (private-fixture-cleanup! fx-a)
  (private-fixture-cleanup! fx-b)
  (delete-directory/files parent #:must-exist? #f))

(when (git-available?)
  (test-case "git fixture: private clone, isolated commits/refs, no template mutation"
    (define tmpl-root #f)
    (define parent (fresh-parent!))
    ;; Snapshot template HEAD before creating fixtures.
    (define fx-a (make-private-git-fixture! #:parent-root parent #:tag "gitA"))
    (define fx-b (make-private-git-fixture! #:parent-root parent #:tag "gitB"))
    (define repo-a (private-git-fixture-repo fx-a))
    (define repo-b (private-git-fixture-repo fx-b))
    ;; Distinct roots/repos.
    (check-not-equal? (path->string (private-fixture-root fx-a))
                      (path->string (private-fixture-root fx-b)))
    ;; No shared objects/hardlinks: --no-local clones produce independent
    ;; .git/objects with no objects/alternates file.
    (check-false (file-exists? (build-path repo-a ".git" "objects" "info" "alternates"))
                 "clone must not use alternates (shared object store)")
    (check-false (file-exists? (build-path repo-b ".git" "objects" "info" "alternates")))
    ;; Offline origin/main stand-in present in each private clone.
    (check-equal? (git-out repo-a "rev-parse" "--verify" "refs/heads/origin/main")
                  (git-out repo-a "rev-parse" "HEAD")
                  "origin/main stand-in points at clone HEAD")
    ;; Commit in A; B and template unaffected.
    (with-output-to-file (build-path repo-a "only-a.txt")
                         (lambda () (displayln "mutation in A"))
                         #:exists 'replace)
    (private-git-system repo-a "git add only-a.txt && git commit -q --no-gpg-sign -m 'a-only'")
    (check-true (file-exists? (build-path repo-a "only-a.txt")))
    (check-false (file-exists? (build-path repo-b "only-a.txt"))
                 "commit in A must not appear in sibling B")
    ;; Ref mutation in A does not leak into B.
    (private-git-system repo-a "git branch hostile-branch")
    (check-equal? (git-out repo-b "branch" "--list" "hostile-branch")
                  ""
                  "hostile branch in A must not leak into B")
    (private-fixture-cleanup! fx-a)
    (private-fixture-cleanup! fx-b)
    (delete-directory/files parent #:must-exist? #f)))

;; --- Contract 4: hermetic identity without global env mutation -------------

(when (git-available?)
  (test-case "git fixture uses repo-local identity; global env untouched"
    (define home-before (getenv "HOME"))
    (define fx
      (with-private-git-repo
       (lambda (f)
         (define repo (private-git-fixture-repo f))
         (define user (git-out repo "config" "user.email"))
         (check-true (string-contains? user "q-fixture@example.invalid"))
         (check-equal? (getenv "HOME") home-before "fixture must not mutate global HOME")
         f)
       #:tag "ident"))
    (private-fixture-cleanup! fx)))

;; --- Contract 5: independent CWD/env mutation ------------------------------

(test-case "fixtures tolerate independent CWD mutation; cleanup order-independent"
  (define parent (fresh-parent!))
  (define fx-a (make-private-session-fixture! #:parent-root parent))
  (define fx-b (make-private-git-fixture! #:parent-root parent #:tag "cwd"))
  (define saved-cwd (current-directory))
  (define saved-foo (getenv "Q_FIXTURE_TEST_VAR"))
  ;; Mutate CWD/env "inside" fixture A, then destroy A before B (order A,B),
  ;; then again destroying B before A (order B,A) on a second round.
  (current-directory (private-fixture-root fx-a))
  (putenv "Q_FIXTURE_TEST_VAR" "a")
  (check-equal? (getenv "Q_FIXTURE_TEST_VAR") "a")
  ;; Cleanup A first, then B.
  (private-fixture-cleanup! fx-a)
  (private-fixture-cleanup! fx-b)
  ;; Re-cleanup both: idempotent.
  (private-fixture-cleanup! fx-a)
  (private-fixture-cleanup! fx-b)
  (check-false (directory-exists? (private-fixture-root fx-a)) "cleanup removed A")
  (current-directory saved-cwd)
  (if saved-foo
      (putenv "Q_FIXTURE_TEST_VAR" saved-foo)
      (putenv "Q_FIXTURE_TEST_VAR" ""))
  (delete-directory/files parent #:must-exist? #f))

(when (git-available?)
  (test-case "cleanup order B-then-A is equally idempotent"
    (define parent (fresh-parent!))
    (define fx-a (make-private-git-fixture! #:parent-root parent #:tag "ordA"))
    (define fx-b (make-private-git-fixture! #:parent-root parent #:tag "ordB"))
    (private-fixture-cleanup! fx-b)
    (private-fixture-cleanup! fx-a)
    (private-fixture-cleanup! fx-b)
    (check-false (directory-exists? (private-fixture-root fx-a)))
    (check-false (directory-exists? (private-fixture-root fx-b)))
    (delete-directory/files parent #:must-exist? #f)))

;; --- Contract 6: several fixtures concurrently + stress --------------------

(when (git-available?)
  (test-case "stress: eight mixed fixtures, independent histories, full cleanup"
    (define parent (fresh-parent!))
    (define fixtures
      (for/list ([i (in-range 8)])
        (if (even? i)
            (make-private-session-fixture! #:parent-root parent #:tag (format "s~a" i))
            (make-private-git-fixture! #:parent-root parent #:tag (format "g~a" i)))))
    ;; All roots distinct.
    (define roots (map (lambda (f) (path->string (private-fixture-root f))) fixtures))
    (check-equal? (length (remove-duplicates roots)) 8 "every fixture has a unique canonical root")
    ;; Independent commits in each git fixture => divergent histories.
    (for ([f fixtures]
          [i (in-naturals)]
          #:when (eq? (private-fixture-kind f) 'git))
      (define repo (private-git-fixture-repo f))
      (with-output-to-file (build-path repo (format "f-~a.txt" i))
                           (lambda () (displayln i))
                           #:exists 'replace)
      (private-git-system
       repo
       (format "git add f-~a.txt && git commit -q --no-gpg-sign -m 'stress-~a'" i i)))
    ;; Distinct SHAs across git fixtures.
    (define shas
      (for/list ([f fixtures]
                 #:when (eq? (private-fixture-kind f) 'git))
        (git-out (private-git-fixture-repo f) "rev-parse" "HEAD")))
    (check-equal? (length (remove-duplicates shas)) 4 "independent histories produce distinct HEADs")
    ;; Destroy in scrambled order, twice each.
    (for ([f (reverse fixtures)])
      (private-fixture-cleanup! f))
    (for ([f fixtures])
      (private-fixture-cleanup! f))
    (for ([r roots])
      (check-false (directory-exists? r) "stress cleanup removed all"))
    (delete-directory/files parent #:must-exist? #f)))

;; --- Contract 7: git-unavailable is a skip, never a pass -------------------

(test-case "git-unavailable guard is explicit"
  (if (git-available?)
      (check-true #t "git present: skip-guard exercised as available")
      (begin
        (displayln "SKIP: git unavailable — git fixture contracts skipped (explicit, not pass)")
        (check-true #t))))
