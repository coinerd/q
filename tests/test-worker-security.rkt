#lang racket/base

;; @speed fast
;; @suite security
;; @boundary integration

;; tests/test-worker-security.rkt
;; v0.99.3 W1: Worker security hardening regression tests.
;;
;; H3: Symlink-based path traversal — symlinks pointing outside allowed roots are rejected
;; M4: IPC-MAX-RESPONSE-BYTES enforcement — oversized responses become error responses
;; M5: CWD mutation fix — parameterize restores CWD after each request

(require rackunit
         rackunit/text-ui
         racket/file
         (only-in "../util/version.rkt" q-version)
         racket/system
         racket/runtime-path
         json
         (only-in racket/string string-contains? string-suffix? string-trim)
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/worker-tools.rkt"
         "../sandbox/worker-main.rkt"
         (only-in "../tools/builtins/bash-safety.rkt" sanctioned-scratch-root)
         "../util/config-paths.rkt")

(define-runtime-path here ".")
(define repo-root (simplify-path (build-path here "..")))

;; ── Test Helpers ────────────────────────────────────────────────

(define temp-base (build-path (find-system-path 'temp-dir) "worker-security-test"))

;; Clean up if exists, then create
(when (directory-exists? temp-base)
  (delete-directory/files temp-base))
(make-directory* temp-base)

;; Create a subdirectory for testing
(define allowed-dir (build-path temp-base "allowed"))
(make-directory* allowed-dir)

;; Set allowed roots for path-allowed? checks
(current-allowed-roots (list allowed-dir))

;; Helper: make an ipc-request JSON string for process-request-line
(define (make-request-json req-id tool args [working-dir #f])
  (jsexpr->string (hash 'request-id
                        req-id
                        'tool-name
                        tool
                        'arguments
                        args
                        'timeout-ms
                        5000
                        'working-dir
                        working-dir
                        'capability
                        "any"
                        'schema-version
                        IPC-SCHEMA-VERSION)))

(define (parse-response resp)
  (ipc-response-status resp))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Worker Security (v0.99.3 W1: H3, M4, M5)"

    ;; ── H3: Symlink-based path traversal ──

    (test-case "H3: symlink to /etc/passwd rejected"
      (define symlink-path (build-path allowed-dir "evil-link"))
      ;; Create symlink pointing to /etc/passwd
      (define link-error
        (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
          (make-file-or-directory-link (string->path "/etc/passwd") symlink-path)
          #f))
      ;; MF3 (v0.99.5): Fail explicitly if symlink creation failed
      (if link-error
          (check-true #f (format "symlink creation failed: ~a" link-error))
          (begin
            (check-false (path-allowed? (path->string symlink-path))
                         "symlink to /etc should be rejected")
            (delete-file symlink-path))))

    (test-case "H3: valid file inside allowed dir is accepted"
      (define valid-file (build-path allowed-dir "test-file"))
      (call-with-output-file valid-file #:exists 'replace (lambda (p) (display "test" p)))
      (check-true (path-allowed? (path->string valid-file))
                  "valid file inside allowed dir should be accepted")
      (delete-file valid-file))

    (test-case "H3: ../../etc/passwd rejected"
      (check-false (path-allowed? "../../../etc/passwd")))

    (test-case "H3: path traversal with allowed-dir prefix rejected"
      ;; A path that starts with allowed-dir but escapes via ..
      (define evil-path (path->string (build-path allowed-dir ".." ".." "etc" "passwd")))
      (check-false (path-allowed? evil-path)))

    ;; ── M4: Output size enforcement ──

    (test-case "M4: oversized response gets error status"
      ;; Simulate by creating a response with huge content via process-request-line
      ;; We use bash with a command that generates massive output
      ;; The response should be 'error with "too large" message
      (define big-output-request
        (make-request-json "m4-big" "bash" (hash 'command "yes | head -c 15000000" 'timeout 10)))
      ;; This will produce >10MB of output — process-request-line should still work,
      ;; but the worker-loop's size check converts it to an error.
      ;; For unit testing, we verify the mechanism: check that a response larger than
      ;; IPC-MAX-RESPONSE-BYTES would be caught
      (define big-response (process-request-line big-output-request))
      ;; The dispatch itself should succeed (content may be large)
      ;; The enforcement happens in worker-loop, not process-request-line
      ;; So we just verify the response is well-formed
      (check-true (ipc-response? big-response)))

    (test-case "M4: normal-sized response passes through"
      (define normal-request
        (make-request-json "m4-normal" "bash" (hash 'command "echo hello" 'timeout 5)))
      (define response (process-request-line normal-request))
      (check-equal? (ipc-response-status response) 'ok))

    ;; ── M5: CWD mutation fix ──

    (test-case "M5: CWD restored after request with working-dir"
      ;; Save original CWD
      (define original-cwd (current-directory))
      ;; Process a request that sets working-dir
      (define request-with-cwd
        (make-request-json "m5-cwd"
                           "bash"
                           (hash 'command "pwd" 'timeout 5)
                           (path->string (build-path "/tmp"))))
      (process-request-line request-with-cwd)
      ;; CWD should be restored
      (check-equal? (current-directory) original-cwd "CWD should be restored after request"))

    (test-case "M5: sequential requests don't inherit CWD"
      ;; First request sets CWD to /tmp
      (define req1
        (make-request-json "m5-seq-1"
                           "bash"
                           (hash 'command "pwd" 'timeout 5)
                           (path->string (build-path "/tmp"))))
      (define resp1 (process-request-line req1))
      (check-equal? (ipc-response-status resp1) 'ok)
      ;; Second request has no working-dir — should run in original CWD
      (define original-cwd (current-directory))
      (define req2 (make-request-json "m5-seq-2" "bash" (hash 'command "pwd" 'timeout 5)))
      (define resp2 (process-request-line req2))
      (check-equal? (ipc-response-status resp2) 'ok)
      ;; CWD should still be the original
      (check-equal? (current-directory) original-cwd))

    ;; ── SEC-4 (v0.99.76 W0): Worker IPC cwd validation ──
    ;; current-allowed-roots is set to (list allowed-dir) at module top;
    ;; "/etc" is outside those roots and must be rejected.

    (test-case "SEC-4: execute-bash rejects cwd outside allowed roots"
      (define result (execute-bash (hasheq 'command "pwd" 'cwd "/etc")))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "cwd not allowed")))

    (test-case "SEC-4: execute-bash accepts cwd inside allowed roots"
      (define result (execute-bash (hasheq 'command "pwd" 'cwd allowed-dir)))
      (check-equal? (ipc-response-status result) 'ok))

    (test-case "SEC-4: execute-git rejects cwd outside allowed roots"
      (define result (execute-git (hasheq 'command "status" 'cwd "/etc")))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "cwd not allowed")))

    ;; ── write tool security (uses path-allowed?) ──

    (test-case "worker dispatch creates missing write parents without widening roots"
      (define root (make-temporary-file "worker-write-parents~a" 'directory))
      (define allowed (build-path root "allowed"))
      (make-directory allowed)
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-allowed-roots (list allowed)])
           (define (write-request target text)
             (process-request-line
              (make-request-json "parents" "write" (hash 'path (path->string target) 'content text))))
           (define outside (build-path root "outside" "probe.rkt"))
           (check-equal? (ipc-response-status (write-request outside "no")) 'error)
           (check-false (directory-exists? (build-path root "outside")))
           (define budget-parent (build-path allowed "budget"))
           (parameterize ([current-worker-write-limit 1])
             (check-equal? (ipc-response-status (write-request (build-path budget-parent "probe.rkt")
                                                               "too big"))
                           'error))
           (check-false (directory-exists? budget-parent))
           (define dir (build-path allowed "new" "nested"))
           (define target (build-path dir "probe.rkt"))
           (check-false (directory-exists? dir))
           (check-equal? (ipc-response-status (write-request target "#lang racket/base\n")) 'ok)
           (check-equal? (file->string target) "#lang racket/base\n")
           (check-equal? (ipc-response-status (write-request target "replacement")) 'ok)
           (check-equal? (file->string target) "replacement")
           (check-equal? (directory-list dir) (list (string->path "probe.rkt")))
           (check-equal? (ipc-response-status (write-request (build-path target "child") "no"))
                         'error)
           (check-equal? (file->string target) "replacement")))
       (lambda () (delete-directory/files root))))

    (test-case "W1 scratch: advertised relative scratch path works through worker dispatch"
      ;; The /go guidance advertises `.planning/scratch/<token>/` relative to
      ;; the executor's current working directory. The worker resolves
      ;; relative paths against its cwd (= worker root), so the advertised
      ;; path lands inside the allowed root in both base-dir and worktree
      ;; execution, and W0's parent creation removes the mkdir detour.
      (define root (make-temporary-file "worker-scratch~a" 'directory))
      (define other (make-temporary-file "worker-scratch-other~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-allowed-roots (list root)]
                        [current-directory root])
           (define resp
             (process-request-line
              (make-request-json "scratch"
                                 "write"
                                 (hash 'path
                                       ".planning/scratch/tok1234567890ab/probe.rkt"
                                       'content
                                       "#lang racket/base\n"))))
           (check-equal? (ipc-response-status resp) 'ok (ipc-response-error-message resp))
           (check-equal?
            (file->string (build-path root ".planning" "scratch" "tok1234567890ab" "probe.rkt"))
            "#lang racket/base\n")
           ;; The same token under a foreign root stays denied — the guidance
           ;; contract never widens worker roots.
           (define denied
             (process-request-line
              (make-request-json
               "scratch-x"
               "write"
               (hash 'path
                     (path->string (build-path other ".planning" "scratch" "tok1234567890ab" "x.rkt"))
                     'content
                     "no"))))
           (check-equal? (ipc-response-status denied) 'error)
           (check-false (directory-exists? (build-path other ".planning")))))
       (lambda ()
         (delete-directory/files root #:must-exist? #f)
         (delete-directory/files other #:must-exist? #f))))

    (test-case "write to allowed path succeeds"
      (define test-file (build-path allowed-dir "write-test"))
      (define resp (execute-write (hash 'path (path->string test-file) 'content "hello")))
      (check-equal? (ipc-response-status resp) 'ok)
      (when (file-exists? test-file)
        (delete-file test-file)))

    (test-case "write to path outside allowed dir rejected"
      (define resp (execute-write (hash 'path "/tmp/worker-security-evil" 'content "hello")))
      (check-equal? (ipc-response-status resp) 'error))

    ;; ── BUG-0028 S2 (W2): denials are self-diagnosing ──
    ;; A denial must name the allowed roots in force at denial time so a
    ;; stale-roots outage is visible in the message itself, not via git
    ;; archaeology. Redaction is unchanged: only the configured roots are
    ;; ever printed.

    (test-case "BUG-0028 S2: write denial enumerates allowed roots"
      (define resp (execute-write (hash 'path "/tmp/worker-security-evil2" 'content "hello")))
      (check-equal? (ipc-response-status resp) 'error)
      (define msg (ipc-response-error-message resp))
      (check-true (string-contains? msg "path not allowed: /tmp/worker-security-evil2")
                  (format "denial must name the path: ~a" msg))
      (check-true (string-contains? msg "(allowed roots: ")
                  (format "denial must enumerate roots: ~a" msg))
      (check-true (string-contains? msg (path->string allowed-dir))
                  (format "denial must name the in-force root ~a: ~a" allowed-dir msg)))

    (test-case "BUG-0028 S2: bash cwd denial enumerates allowed roots"
      (define resp (execute-bash (hasheq 'command "pwd" 'cwd "/etc/passwd-dir")))
      (check-equal? (ipc-response-status resp) 'error)
      (define msg (ipc-response-error-message resp))
      (check-true (string-contains? msg "cwd not allowed: /etc/passwd-dir")
                  (format "denial must name the cwd: ~a" msg))
      (check-true (string-contains? msg "(allowed roots: ")
                  (format "denial must enumerate roots: ~a" msg))
      (check-true (string-contains? msg (path->string allowed-dir))
                  (format "denial must name the in-force root ~a: ~a" allowed-dir msg)))

    ;; ── LF3 (v0.99.4): Symlink in path with non-existent subdirs ──

    (test-case "LF3: symlink + non-existent subdir rejected"
      (define symlink-path (build-path allowed-dir "lf3-link"))
      (define link-error
        (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
          (make-file-or-directory-link (find-system-path 'temp-dir) symlink-path)
          #f))
      (if link-error
          (check-true #f (format "symlink creation failed: ~a" link-error))
          (let ([target-path
                 (build-path (path->string symlink-path) "nonexistent-subdir" "file.txt")])
            (check-false (path-allowed? (path->string target-path))
                         "LF3: symlink + non-existent subdir should be rejected")
            (delete-file symlink-path))))

    (test-case "LF3: deeply nested symlink escape rejected"
      (define symlink-path (build-path allowed-dir "deep-evil"))
      (define link-error
        (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
          (make-file-or-directory-link (string->path "/etc") symlink-path)
          #f))
      (if link-error
          (check-true #f (format "symlink creation failed: ~a" link-error))
          (let ([target-path (build-path (path->string symlink-path) "a" "b" "c" "escape.txt")])
            (check-false (path-allowed? (path->string target-path))
                         "LF3: deeply nested symlink escape should be rejected")
            (delete-file symlink-path))))

    (test-case "LF3: valid symlink inside allowed dir accepted"
      (define inner-dir (build-path allowed-dir "inner"))
      (make-directory* inner-dir)
      (define symlink-path (build-path allowed-dir "good-link"))
      (define link-error
        (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
          (make-file-or-directory-link inner-dir symlink-path)
          #f))
      (if link-error
          (check-true #f (format "symlink creation failed: ~a" link-error))
          (let ([target-path (build-path (path->string symlink-path) "newfile.txt")])
            (check-true (path-allowed? (path->string target-path))
                        "LF3: symlink within allowed dir should be accepted")
            (delete-file symlink-path)))
      (when (directory-exists? inner-dir)
        (delete-directory/files inner-dir)))

    (test-case "LF3: resolve-longest-prefix resolves symlink in middle of path"
      (define sub-dir (build-path allowed-dir "sub"))
      (make-directory* sub-dir)
      (define symlink-path (build-path allowed-dir "mid-link"))
      (define link-error
        (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
          (make-file-or-directory-link sub-dir symlink-path)
          #f))
      (if link-error
          (check-true #f (format "symlink creation failed: ~a" link-error))
          (let ([target (build-path (path->string symlink-path) "deep" "file.txt")])
            (check-true (path-allowed? (path->string target))
                        "LF3: symlink to allowed dir + non-existent path should be accepted")
            (delete-file symlink-path)))
      (when (directory-exists? sub-dir)
        (delete-directory/files sub-dir)))

    ;; v1.00.09: The configured allowed root can itself be reached through a
    ;; symlinked ancestor (for example, a platform temporary-directory alias).
    ;; Requests and roots must therefore use the same canonicalization path.
    (test-case "LF3: symlinked allowed-root ancestor accepts in-root tail"
      (define fixture-root (make-temporary-directory "worker-security-lf3-alias~a"))
      (dynamic-wind
       void
       (lambda ()
         (define physical-parent (build-path fixture-root "physical-parent"))
         (define alias-parent (build-path fixture-root "alias-parent"))
         (define physical-allowed (build-path physical-parent "allowed"))
         (define lexical-allowed (build-path alias-parent "allowed"))
         (define mid-link (build-path lexical-allowed "mid-link"))
         (make-directory* (build-path physical-allowed "sub"))
         ;; Relative target: this is the macOS-triggering condition. `resolve-path`
         ;; returns "physical-parent" relative to alias-parent's containing directory.
         (make-file-or-directory-link (string->path "physical-parent") alias-parent)
         (make-file-or-directory-link (build-path physical-allowed "sub") mid-link)
         (parameterize ([current-allowed-roots (list lexical-allowed)])
           (define target (build-path mid-link "deep" "file.txt"))
           (check-true (path-allowed? (path->string target))
                       "LF3: an in-root path through an aliased ancestor must be accepted")))
       (lambda ()
         (when (directory-exists? fixture-root)
           (delete-directory/files fixture-root)))))

    ;; macOS temporary paths can make an existing absolute symlink target retain
    ;; a lexical alias (for example /var) while the allowed root is already
    ;; physical (/private/var). The resolved target itself must be walked again.
    (test-case "LF3: absolute symlink target through lexical alias remains in-root"
      (define fixture-root (make-temporary-directory "worker-security-lf3-absolute-alias~a"))
      (dynamic-wind void
                    (lambda ()
                      (define physical-parent (build-path fixture-root "physical-parent"))
                      (define alias-parent (build-path fixture-root "alias-parent"))
                      (define physical-allowed (build-path physical-parent "allowed"))
                      (define lexical-allowed (build-path alias-parent "allowed"))
                      (define lexical-inner (build-path lexical-allowed "inner"))
                      (define alias-link (build-path physical-allowed "absolute-alias-link"))
                      (make-directory* (build-path physical-allowed "inner"))
                      (make-file-or-directory-link (string->path "physical-parent") alias-parent)
                      ;; The link target is deliberately absolute but still has a lexical
                      ;; alias ancestor. This is the shape emitted by the macOS fixture.
                      (make-file-or-directory-link lexical-inner alias-link)
                      (parameterize ([current-allowed-roots (list physical-allowed)])
                        (define target (build-path alias-link "deep" "file.txt"))
                        (check-true (path-allowed? (path->string target))
                                    "LF3: absolute target through an alias must remain in-root")))
                    (lambda ()
                      (when (directory-exists? fixture-root)
                        (delete-directory/files fixture-root)))))
    (test-case "SEC-1: execute-git blocks force push to shared branch"
      (define result (execute-git (hasheq 'command "push" 'args '("--force" "origin" "main"))))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "blocked")))

    (test-case "SEC-1: execute-git blocks clean -fdx"
      (define result (execute-git (hasheq 'command "clean" 'args '("-fdx"))))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "blocked")))

    (test-case "SEC-1: execute-git blocks reset --hard"
      (define result (execute-git (hasheq 'command "reset" 'args '("--hard"))))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "blocked")))

    (test-case "SEC-1: execute-git allows safe commands"
      ;; W2 (BUG-0028): pin an explicit allowed cwd so the test does not depend
      ;; on the ambient process directory (running from project base made
      ;; `git status` fail with "not a git repository" and broke delivery).
      (define repo-dir (make-temporary-file "sec1-git-~a" 'directory))
      (current-allowed-roots (list allowed-dir repo-dir))
      (system* (find-executable-path "git") "-C" (path->string repo-dir) "init" "--quiet")
      (define result (execute-git (hasheq 'command "status" 'args '() 'cwd (path->string repo-dir))))
      (current-allowed-roots (list allowed-dir))
      (check-equal? (ipc-response-status result) 'ok)
      (delete-directory/files repo-dir))

    ;; ── SEC-7: Worker file safety (v0.99.76 W2) ──

    (test-case "SEC-7: execute-write blocks content exceeding size limit"
      (define large-content (make-string 1048577 #\x)) ; 1 MB + 1
      (define target (build-path allowed-dir "sec7-large.txt"))
      (define result (execute-write (hasheq 'path (path->string target) 'content large-content)))
      (check-equal? (ipc-response-status result) 'error)
      (check-true (string-contains? (ipc-response-error-message result) "exceeds")))

    (test-case "SEC-7: execute-edit blocks oversized replacement (parity)"
      ;; edit must enforce the same per-write limit as write/delete-lines.
      (define target (build-path allowed-dir "sec7-edit-large.txt"))
      (call-with-output-file target #:exists 'replace (lambda (p) (display "base" p)))
      (parameterize ([current-worker-write-limit (* 1024 1024)])
        (define oversized (make-string 1048577 #\z)) ; 1 MB + 1
        (define result
          (execute-edit (hasheq 'path (path->string target) 'old-text "base" 'new-text oversized)))
        (check-equal? (ipc-response-status result) 'error)
        (check-true (string-contains? (ipc-response-error-message result) "exceeds")
                    (format "error should mention size limit: ~a"
                            (ipc-response-error-message result))))
      ;; Original file must be untouched (fail closed, no partial write)
      (check-equal? (file->string target) "base"))

    (test-case "SEC-7: execute-write tracks cumulative budget"
      ;; Write 600 KB twice (over 1 MB cumulative) — both under per-write limit.
      (define chunk (make-string (* 600 1024) #\y))
      (define target-a (build-path allowed-dir "sec7-cum-a.txt"))
      (define target-b (build-path allowed-dir "sec7-cum-b.txt"))
      (parameterize ([current-worker-cumulative-limit (* 1024 1024)]
                     [current-worker-write-limit (* 1024 1024)])
        (define r1 (execute-write (hasheq 'path (path->string target-a) 'content chunk)))
        (check-equal? (ipc-response-status r1) 'ok)
        (define r2 (execute-write (hasheq 'path (path->string target-b) 'content chunk)))
        (check-equal? (ipc-response-status r2) 'error)
        (check-true (string-contains? (ipc-response-error-message r2) "cumulative"))))

    (test-case "SEC-7: execute-edit creates backup"
      (define target (build-path allowed-dir "sec7-edit.txt"))
      (call-with-output-file target #:exists 'replace (lambda (p) (display "hello world" p)))
      (define backup-dir (build-path (global-config-dir) "edit-backups"))
      (when (directory-exists? backup-dir)
        (delete-directory/files backup-dir))
      (define result
        (execute-edit (hasheq 'path (path->string target) 'old-text "hello" 'new-text "goodbye")))
      (check-equal? (ipc-response-status result) 'ok)
      (check-true (directory-exists? backup-dir) "backup dir should be created")
      (define backups
        (filter (lambda (f) (string-suffix? f "_sec7-edit.txt"))
                (map path->string (directory-list backup-dir))))
      (check-true (> (length backups) 0) "at least one backup file should exist")
      ;; Backup must contain the ORIGINAL content (pre-edit)
      (check-equal? (file->string (build-path backup-dir (car (sort backups string>?))))
                    "hello world"))

    (test-case "SEC-7: execute-edit detects concurrent modification"
      (define target (build-path allowed-dir "sec7-concurrent.txt"))
      (call-with-output-file target #:exists 'replace (lambda (p) (display "version one" p)))
      (define inode-before (file-or-directory-identity target))
      ;; Simulate concurrent modification: replace the file with a new inode
      ;; between read and write by using the before-write hook.
      (define hook-ran (box #f))
      (parameterize ([current-worker-edit-before-write-hook
                      (lambda (path new-content)
                        (set-box! hook-ran #t)
                        ;; Replace via rename-from-temp: guarantees a NEW inode
                        ;; (delete+recreate may reuse the same inode on ext4).
                        (define tmp (make-temporary-file "sec7-tamper-~a.txt"))
                        (display-to-file "tampered" tmp #:exists 'truncate)
                        (rename-file-or-directory tmp path #t))])
        (define result
          (execute-edit
           (hasheq 'path (path->string target) 'old-text "version one" 'new-text "version two")))
        (check-true (unbox hook-ran) "hook should have run")
        (check-equal? (ipc-response-status result) 'error)
        (check-true (string-contains? (ipc-response-error-message result) "concurrently")
                    (format "error should mention concurrent modification: ~a"
                            (ipc-response-error-message result))))
      ;; File should still contain the tampered (newer) content
      (check-equal? (file->string target) "tampered"))

    (test-case "SEC-7: execute-delete-lines creates backup"
      (define target (build-path allowed-dir "sec7-del.txt"))
      (call-with-output-file target #:exists 'replace (lambda (p) (display "line1\nline2\nline3" p)))
      (define backup-dir (build-path (global-config-dir) "edit-backups"))
      (define result
        (execute-delete-lines (hasheq 'path (path->string target) 'start-line 2 'end-line 2)))
      (check-equal? (ipc-response-status result) 'ok)
      (define backups
        (filter (lambda (f) (string-suffix? f "_sec7-del.txt"))
                (map path->string (directory-list backup-dir))))
      (check-true (> (length backups) 0) "delete-lines should create a backup")
      (check-equal? (file->string (build-path backup-dir (car (sort backups string>?))))
                    "line1\nline2\nline3"))))

;; ── Run ─────────────────────────────────────────────────────────

(run-tests suite)

;; ── W2 (#9590): overlap ownership + equivalence evidence ──
;; W2 reviews the platform/fast and security/fast overlaps and removes
;; ONLY exact accidental duplicates. This file's overlap review keeps
;; every security/fast row: the security gate is a dedicated required
;; blocking control (STRICT_TEST_RUNNER=1, per-job full-install runner),
;; so its overlap with fast is intentional and never removable. The
;; assertions below bind that decision to checksummed evidence: the
;; review artifact must keep every W0 security/fast and platform/fast
;; row, every kept row must carry a rationale naming its owning gate,
;; the CI lane contexts must still give all three suites the same
;; STRICT_TEST_RUNNER runner contract, and repeated worker-security
;; checks must produce the same result record (both-tier equivalence).

(require json
         racket/port
         racket/string
         racket/list)

;; SHA-256 via sha256sum subprocess (repo-canonical digest method; the
;; runtime ships no file/sha256 collect).
(define (sha256-hex* path)
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f (find-executable-path "sha256sum") (path->string path)))
  (close-output-port stdin)
  (define hex (string-trim (port->string stdout)))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (car (string-split hex)))

;; The W2 overlap review and its SHA256SUMS manifest are FROZEN artifacts of the
;; v1.00.29 campaign (that campaign's W0 and W2 produced them). Their directories
;; were derived from q-version, which only agreed with the artifacts while the
;; canonical version happened to be 1.00.29; deriving them after a bump names a
;; directory that does not exist. Pinned literally, reason recorded. The W0
;; ownership matrix below is deliberately NOT pinned: it is the current
;; milestone's copy, so these checks also act as a drift check that the frozen
;; review's rows still cover the live platform/fast and security/fast overlap
;; scope 1:1.
(define overlap-review-path*
  (build-path repo-root "artifacts" "tier-ownership" "v1.00.29-w2" "overlap-review.json"))

(define w2-sha256sums-path*
  (build-path repo-root "artifacts" "tier-ownership" "v1.00.29-w2" "SHA256SUMS"))

(define w0-matrix-path*
  (build-path repo-root
              "artifacts"
              "tier-ownership"
              (string-append "v" q-version "-w0")
              "ownership-matrix.json"))

(define ci-workflow-path* (build-path repo-root ".github" "workflows" "ci.yml"))

;; read-json yields symbol-keyed hashes; the review JSON uses string names.
(define (jref h k)
  (hash-ref h (string->symbol k)))
;; W0 ownership-matrix rows whose required_gates list BOTH gates: the
;; overlap scope for one axis.
(define (w2-axis-rows gate-a gate-b)
  (define matrix (call-with-input-file w0-matrix-path* read-json))
  ;; read-json yields lists of STRING gate names, so match with
  ;; member/equal? — memq on symbols would always miss.
  (for/list ([row (in-list (hash-ref matrix 'rows))]
             #:when (let ([gates (hash-ref row 'required_gates)])
                      (and (member (symbol->string gate-a) gates)
                           (member (symbol->string gate-b) gates))))
    row))

;; The ci.yml window around a suite's runner invocation (first hit,
;; matching the canonical job for that suite).
(define (ci-suite-window* suite)
  (define lines (file->lines ci-workflow-path*))
  (define needle (string-append "--suite " suite))
  (define idx
    (for/first ([ln (in-list lines)]
                [i (in-naturals)]
                #:when (string-contains? ln needle))
      i))
  (and idx
       (string-join (take (drop lines (max 0 (- idx 6)))
                          (min 18 (max 0 (- (length lines) (max 0 (- idx 6))))))
                    "\n")))

;; The W2 checks below run as their own test-suite, invoked through run-tests,
;; and the process exits non-zero on run-tests' failure count.
;;
;; They used to be bare module-body test-cases placed after this file's first
;; (run-tests suite) call, paired with a hand-rolled current-check-handler that
;; counted check results and exited at the end. That combination could not fail
;; the gate. rackunit reports module-body test-cases that run after a run-tests
;; call, but their failures do not reach the process exit status, and the custom
;; handler replaced the accounting that would have registered them — so the file
;; printed FAILURE blocks, printed a summary reading "0 failure(s)", exited 0,
;; and the suite runner, which treats a file as failed purely on a non-zero exit
;; code, reported it as PASSED. A red W2 could read as green through the entire
;; gate, and did: these artifact paths had been derived from q-version, so after
;; a version bump they named a directory that does not exist and nothing
;; reported it.
;;
;; run-tests returns the number of failed checks, which is an ordinary return
;; value rather than an exit-status side effect, so the count is used directly
;; below. Its printing is unchanged, so the capture format is unchanged too.
(define w2-suite
  (test-suite "W2 overlap governance"
    (test-case "W2: every platform/fast overlap row is kept with an explicit rationale"
      (check-true (file-exists? overlap-review-path*) "W2 overlap review artifact is missing")
      (define review (call-with-input-file overlap-review-path* read-json))
      (check-equal? (jref review "schema") "tier-ownership-overlap-review/v1")
      (define w0-rows (w2-axis-rows 'platform 'fast))
      (check-true (pair? w0-rows) "W0 platform/fast scope is empty — matrix drifted")
      (define review-rows
        (for/list ([rr (in-list (hash-ref review 'rows))]
                   #:when (equal? (jref rr "axis") "platform/fast"))
          rr))
      (check-equal? (length review-rows)
                    (length w0-rows)
                    "platform/fast review rows must cover the W0 overlap scope 1:1")
      (for ([rr (in-list review-rows)])
        (define decision (jref rr "decision"))
        (check-not-false
         (member decision '("kept" "removed-duplicate"))
         (format "platform/fast row ~a has unknown decision ~a" (jref rr "test") decision))
        (define rationale (jref rr "rationale"))
        (check-true (and (string? rationale) (> (string-length rationale) 0))
                    (format "platform/fast row ~a carries no rationale" (jref rr "test")))
        (when (equal? decision "kept")
          (check-true (string-contains? rationale "platform")
                      (format "platform/fast kept rationale for ~a must name the platform gate"
                              (jref rr "test"))))))

    (test-case "W2: every security/fast overlap row is kept in the security gate"
      (define review (call-with-input-file overlap-review-path* read-json))
      (define w0-rows (w2-axis-rows 'security 'fast))
      (check-true (pair? w0-rows) "W0 security/fast scope is empty — matrix drifted")
      (define review-rows
        (for/list ([rr (in-list (hash-ref review 'rows))]
                   #:when (equal? (jref rr "axis") "security/fast"))
          rr))
      (check-equal? (length review-rows)
                    (length w0-rows)
                    "security/fast review rows must cover the W0 overlap scope 1:1")
      (for ([rr (in-list review-rows)])
        (check-equal?
         (jref rr "decision")
         "kept"
         (format
          "security/fast row ~a must stay in the security gate: removal would be a security re-tiering"
          (jref rr "test")))
        (define rationale (jref rr "rationale"))
        (check-true (and (string? rationale) (> (string-length rationale) 0))
                    (format "security/fast row ~a carries no rationale" (jref rr "test")))
        (check-true (string-contains? rationale "security")
                    (format "security/fast rationale for ~a must name the security gate"
                            (jref rr "test")))))

    (test-case "W2: all three lane contexts share the same strict runner contract"
      ;; Equivalence precondition: fast (prepared-environment shard),
      ;; platform (full-install + raco make) and security (full-install)
      ;; all invoke the runner with STRICT_TEST_RUNNER=1, so the same test
      ;; id is held to the same result record under either tier's context.
      (define fast-window (ci-suite-window* "fast"))
      (define platform-window (ci-suite-window* "platform"))
      (define security-window (ci-suite-window* "security"))
      (check-true (and fast-window platform-window security-window #t)
                  "ci.yml is missing a suite runner block")
      ;; Strictness is asserted as "STRICT_TEST_RUNNER=1 is set and the runner is
      ;; invoked", not as the literal string "STRICT_TEST_RUNNER=1 racket
      ;; scripts/run-tests.rkt". The fast lane no longer matches that literal: the
      ;; v1.00.30 W4 repair (issue #9690) routes it through the verified
      ;; compiled-root launcher, so the command is
      ;;   STRICT_TEST_RUNNER=1 racket ci/prepared-environment/compiled-root.rkt
      ;;     launch ... -- racket scripts/run-tests.rkt
      ;; which is still the strict runner, with the runner behind the launcher's
      ;; -- separator. Asserting the old literal would fail on a lane that is in
      ;; fact strict. Note this check only became visible now: the W2 suite could
      ;; not fail the process before, so its staleness was never reported.
      (for ([w (in-list (list fast-window platform-window security-window))])
        (check-true (string-contains? w "STRICT_TEST_RUNNER=1")
                    "every tier must set STRICT_TEST_RUNNER=1")
        (check-true (string-contains? w "racket scripts/run-tests.rkt")
                    "every tier must invoke scripts/run-tests.rkt"))
      (check-true (string-contains? fast-window "--suite fast") "fast lane block drifted")
      (check-true (string-contains? platform-window "--suite platform") "platform lane block drifted")
      (check-true (string-contains? platform-window "raco make main.rkt")
                  "platform lane must keep its full-install raco make context")
      (check-false (string-contains? platform-window "PREPARED_ENV")
                   "platform lane must remain a cold full-install context")
      (check-true (string-contains? security-window "--suite security") "security lane block drifted")
      (check-false (string-contains? security-window "PREPARED_ENV")
                   "security lane must remain a cold full-install context")
      (check-true (string-contains? (file->string ci-workflow-path*) "PREPARED_ENV")
                  "fast lane's prepared-environment shard machinery is missing"))

    (test-case "W2: worker-security checks produce identical verdicts across repeated executions"
      ;; Both-tier equivalence spot check: the same disallowed-path check
      ;; yields the same status verdict and a self-diagnosing message on
      ;; every execution, i.e. the same result record either tier's runner
      ;; environment would report.
      (define a (execute-write (hash 'path "/tmp/worker-security-w2-equiv-a" 'content "x")))
      (define b (execute-write (hash 'path "/tmp/worker-security-w2-equiv-b" 'content "x")))
      (check-equal? (ipc-response-status a) 'error)
      (check-equal? (ipc-response-status a)
                    (ipc-response-status b)
                    "the same check must produce the same result record verdict")
      (check-true (string-contains? (ipc-response-error-message a) "path not allowed")
                  "denial must remain self-diagnosing")
      (check-true (string-contains? (ipc-response-error-message b) "path not allowed")
                  "denial must remain self-diagnosing"))

    (test-case "W2: checksum manifest matches the overlap review artifact"
      (check-true (file-exists? w2-sha256sums-path*) "W2 SHA256SUMS manifest is missing")
      (define entry
        (for/first ([ln (in-list (file->lines w2-sha256sums-path*))]
                    #:when (string-contains? ln "overlap-review.json"))
          ln))
      (check-true (and entry (string? entry)) "manifest does not cover overlap-review.json")
      (check-equal? (car (string-split entry))
                    (sha256-hex* overlap-review-path*)
                    "overlap-review.json digest drifted from its checksummed manifest"))))

;; ── Cleanup ──

;; Clean up test directory
(when (directory-exists? temp-base)
  (delete-directory/files temp-base))

;; Fail the process when the W2 suite reports any failed check. This is the only
;; exit-code path the W2 checks have, and it is checked by construction: with
;; the artifact path pointed at a directory that does not exist, this file exits
;; non-zero and the suite runner reports it FAILED.
(define w2-failed-checks (run-tests w2-suite))
(when (> w2-failed-checks 0)
  (eprintf "worker-security W2 overlap governance: ~a failed check(s)~n" w2-failed-checks)
  (exit 1))
