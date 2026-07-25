#lang racket/base

;; tests/test-project-memory-identity.rkt
;; W5A (#8942): Canonical project identity — pure derivation logic.
;; Covers: origin URL canonicalization, gitdir parsing/classification,
;; key computation, worktree aliasing, storage path derivation,
;; namespace safety (fail-closed), and full I/O entrypoint.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../runtime/memory/project-identity.rkt")

(define-test-suite
 project-identity-suite
 ;; ── Struct & constructor ──
 (test-case "make-project-identity constructs with required fields"
   (define id
     (make-project-identity #:key "abc123"
                            #:root "/some/proj"
                            #:kind 'git-main
                            #:origin-url "https://github.com/u/r.git"
                            #:common-dir "/some/proj/.git"))
   (check-true (project-identity? id))
   (check-equal? (project-identity-key id) "abc123")
   (check-equal? (project-identity-root id) "/some/proj")
   (check-equal? (project-identity-kind id) 'git-main)
   (check-equal? (project-identity-origin-url id) "https://github.com/u/r.git")
   (check-equal? (project-identity-common-dir id) "/some/proj/.git"))
 (test-case "make-project-identity rejects empty key (fail-closed invariant)"
   (check-exn exn:fail?
              (lambda ()
                (make-project-identity #:key ""
                                       #:root "/some/p"
                                       #:kind 'plain-directory
                                       #:origin-url #f
                                       #:common-dir #f))))
 ;; ── Origin URL canonicalization ──
 (test-case "canonicalize strips trailing .git"
   (check-equal? (canonicalize-origin-url "https://github.com/u/r.git") "github.com/u/r"))
 (test-case "canonicalize strips trailing slash"
   (check-equal? (canonicalize-origin-url "https://github.com/u/r/") "github.com/u/r"))
 (test-case "canonicalize converts SSH git@ host to path form"
   (check-equal? (canonicalize-origin-url "git@github.com:u/r.git") "github.com/u/r"))
 (test-case "canonicalize lowercases host"
   (check-equal? (canonicalize-origin-url "https://GitHub.COM/U/R.git") "github.com/u/r"))
 (test-case "canonicalize handles ssh with nested group"
   (check-equal? (canonicalize-origin-url "git@gitlab.example.com:group/sub/repo.git")
                 "gitlab.example.com/group/sub/repo"))
 (test-case "canonicalize returns #f for empty/whitespace"
   (check-false (canonicalize-origin-url ""))
   (check-false (canonicalize-origin-url "   ")))
 (test-case "canonicalize is idempotent"
   (define once (canonicalize-origin-url "https://github.com/u/r.git"))
   (check-equal? (canonicalize-origin-url once) once))
 ;; ── Gitdir file parsing & classification ──
 (test-case "parse-gitdir-entry extracts path from 'gitdir:' line"
   (check-equal? (parse-gitdir-entry "gitdir: /abs/path/to/.git/worktrees/wt\n")
                 "/abs/path/to/.git/worktrees/wt"))
 (test-case "parse-gitdir-entry returns #f for non-gitdir content"
   (check-false (parse-gitdir-entry "not a gitdir line"))
   (check-false (parse-gitdir-entry "")))
 (test-case "classify-gitdir-path: main repo"
   (check-equal? (classify-gitdir-path "/some/proj/.git" 'directory) 'git-main))
 (test-case "classify-gitdir-path: linked worktree path"
   (check-equal? (classify-gitdir-path "/some/proj/.git/worktrees/wt" 'directory) 'git-worktree))
 (test-case "classify-gitdir-path: submodule path"
   (check-equal? (classify-gitdir-path "/some/proj/.git/modules/sub" 'directory) 'git-submodule))
 (test-case "worktree-common-dir-from-gitdir resolves common dir"
   (check-equal? (worktree-common-dir-from-gitdir "/some/main/.git/worktrees/wt") "/some/main/.git"))
 (test-case "worktree-common-dir-from-gitdir returns #f for non-worktree"
   (check-false (worktree-common-dir-from-gitdir "/some/proj/.git")))
 ;; ── Key computation (deterministic) ──
 (test-case "compute-project-key prefers origin URL"
   (define k1
     (compute-project-key #:origin-url "https://github.com/u/r.git"
                          #:root "/some/path/a"
                          #:common-dir "/some/path/a/.git"))
   (define k2
     (compute-project-key #:origin-url "https://github.com/u/r.git"
                          #:root "/some/path/b"
                          #:common-dir "/some/path/b/.git"))
   (check-equal? k1 k2)
   (check-true (> (string-length k1) 0)))
 (test-case "compute-project-key differs for different origins"
   (define k1
     (compute-project-key #:origin-url "https://github.com/u/r.git"
                          #:root "/some/p"
                          #:common-dir "/some/p/.git"))
   (define k2
     (compute-project-key #:origin-url "https://github.com/u/other.git"
                          #:root "/some/p"
                          #:common-dir "/some/p/.git"))
   (check-false (equal? k1 k2)))
 (test-case "compute-project-key falls back to common-dir when no origin"
   (define k1 (compute-project-key #:origin-url #f #:root "/some/p" #:common-dir "/some/p/.git"))
   (define k2 (compute-project-key #:origin-url #f #:root "/some/p" #:common-dir "/some/p/.git"))
   (check-equal? k1 k2)
   (check-true (> (string-length k1) 0)))
 (test-case "compute-project-key: SSH and HTTPS same repo -> same key"
   (define k-ssh
     (compute-project-key #:origin-url "git@github.com:u/r.git"
                          #:root "/some/p"
                          #:common-dir "/some/p/.git"))
   (define k-https
     (compute-project-key #:origin-url "https://github.com/u/r.git"
                          #:root "/some/p"
                          #:common-dir "/some/p/.git"))
   (check-equal? k-ssh k-https))
 (test-case "compute-project-key is deterministic"
   (check-equal? (compute-project-key #:origin-url "https://github.com/u/r.git"
                                      #:root "/some/p"
                                      #:common-dir "/some/p/.git")
                 (compute-project-key #:origin-url "https://github.com/u/r.git"
                                      #:root "/some/p"
                                      #:common-dir "/some/p/.git")))
 ;; ── Worktree aliasing (same-project?) ──
 (test-case "same-project? main repo and its worktree alias"
   (define main
     (make-project-identity #:key "k1"
                            #:root "/some/main"
                            #:kind 'git-main
                            #:origin-url "https://github.com/u/r.git"
                            #:common-dir "/some/main/.git"))
   (define wt
     (make-project-identity #:key "k2"
                            #:root "/some/main-wt"
                            #:kind 'git-worktree
                            #:origin-url "https://github.com/u/r.git"
                            #:common-dir "/some/main/.git"))
   (check-true (same-project? main wt)))
 (test-case "same-project? different repos do not alias"
   (define a
     (make-project-identity #:key "ka"
                            #:root "/some/a"
                            #:kind 'git-main
                            #:origin-url "https://github.com/u/a.git"
                            #:common-dir "/some/a/.git"))
   (define b
     (make-project-identity #:key "kb"
                            #:root "/some/b"
                            #:kind 'git-main
                            #:origin-url "https://github.com/u/b.git"
                            #:common-dir "/some/b/.git"))
   (check-false (same-project? a b)))
 (test-case "same-project? submodules are distinct projects"
   (define main
     (make-project-identity #:key "km"
                            #:root "/some/main"
                            #:kind 'git-main
                            #:origin-url "https://github.com/u/main.git"
                            #:common-dir "/some/main/.git"))
   (define sub
     (make-project-identity #:key "ks"
                            #:root "/some/main/sub"
                            #:kind 'git-submodule
                            #:origin-url "https://github.com/u/sub.git"
                            #:common-dir "/some/main/.git/modules/sub"))
   (check-false (same-project? main sub)))
 ;; ── Storage path derivation ──
 (test-case "project-memory-subdir keys off identity key"
   (define id
     (make-project-identity #:key "abcdef"
                            #:root "/some/p"
                            #:kind 'git-main
                            #:origin-url #f
                            #:common-dir "/some/p/.git"))
   (define subdir (project-memory-subdir id "/some/data/q"))
   (check-true (string? subdir))
   (check-true (string-contains? subdir "abcdef"))
   (check-true (string-prefix? subdir "/some/data/q")))
 (test-case "project-memory-subdir is sharded by key prefix"
   (define id
     (make-project-identity #:key "f00d"
                            #:root "/some/p"
                            #:kind 'plain-directory
                            #:origin-url #f
                            #:common-dir #f))
   (define subdir (project-memory-subdir id "/some/data"))
   (check-true (string-contains? subdir "f0")))
 ;; ── Namespace safety (fail-closed) ──
 (test-case "namespace-safe? rejects dot namespace"
   (check-false (namespace-safe? ".")))
 (test-case "namespace-safe? rejects empty/missing namespace"
   (check-false (namespace-safe? ""))
   (check-false (namespace-safe? #f)))
 (test-case "namespace-safe? rejects session-log paths"
   ;; Use /sess prefix (allowed fake) so the lint passes, but this still
   ;; contains the .jsonl + sessions-segment markers we want to reject.
   (check-false (namespace-safe? "/sess/store/sessions/abc.jsonl"))
   (check-false (namespace-safe? "/sess/store/sessions/log.jsonl")))
 (test-case "namespace-safe? accepts canonical project keys"
   (check-true (namespace-safe? "a94a8fe5ccb19ba61c4c"))
   (check-true (namespace-safe? "github.com/u/r")))
 (test-case "quarantine-reason classifies unsafe namespaces"
   (check-equal? (quarantine-reason ".") 'dot-namespace)
   (check-equal? (quarantine-reason "/sess/store/sessions/a.jsonl") 'session-log-path)
   (check-false (quarantine-reason "safekey123")))
 ;; ── Fail-closed: derive-project-identity ──
 (test-case "derive-project-identity returns valid id or #f for temp dir"
   (define tmp (make-temporary-file "q-id-test-~a" 'directory))
   (define id (derive-project-identity #:start-dir tmp))
   (cond
     [id
      (check-true (project-identity? id))
      (check-true (string? (project-identity-key id)))
      (check-true (> (string-length (project-identity-key id)) 0))
      (check-false (equal? (project-identity-key id) "."))]
     [else (check-true #t "fail-closed to #f is acceptable")])
   (delete-directory/files tmp))
 (test-case "derive-project-identity never returns key '.'"
   (define id (derive-project-identity))
   (when id
     (check-false (equal? (project-identity-key id) "."))
     (check-false (equal? (project-identity-root id) ".")))))

(run-tests project-identity-suite)
