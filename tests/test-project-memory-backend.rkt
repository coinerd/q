#lang racket/base

;; tests/test-project-memory-backend.rkt
;; W5B (#8942): Project-keyed isolated memory backend factory.
;; The factory derives a project-keyed memory-root from the canonical
;; project identity (W5A), replacing the unsafe session-dir/temp-dir fallback.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../runtime/memory/project-identity.rkt"
         "../runtime/memory/project-keyed-factory.rkt")

(define (make-test-identity key
                            root
                            #:kind [kind 'git-main]
                            #:origin [origin "https://github.com/u/r.git"]
                            #:common [common "/some/p/.git"])
  (make-project-identity #:key key #:root root #:kind kind #:origin-url origin #:common-dir common))

(define-test-suite
 project-keyed-factory-suite
 ;; ── Path derivation ──
 (test-case "project-keyed-memory-root derives from identity"
   (define id (make-test-identity "abcdef" "/some/p"))
   (define root (project-keyed-memory-root id #:base-data-dir "/some/data/q"))
   (check-true (string? root))
   (check-true (string-contains? root "abcdef"))
   (check-true (string-prefix? root "/some/data/q")))
 (test-case "project-keyed-memory-root is sharded"
   (define id (make-test-identity "f00d" "/some/p"))
   (define root (project-keyed-memory-root id #:base-data-dir "/some/data"))
   (check-true (regexp-match? #rx"projects/f0/" root)))
 (test-case "project-keyed-memory-root differs per identity"
   (define id1 (make-test-identity "aaaa" "/some/a"))
   (define id2 (make-test-identity "bbbb" "/some/b"))
   (define r1 (project-keyed-memory-root id1 #:base-data-dir "/some/data"))
   (define r2 (project-keyed-memory-root id2 #:base-data-dir "/some/data"))
   (check-false (equal? r1 r2)))
 ;; ── Factory: build a real file-jsonl backend keyed by identity ──
 (test-case "make-project-keyed-backend returns a backend"
   (define id (make-test-identity "key1" "/some/p"))
   (define tmp (make-temporary-file "q-factory-~a" 'directory))
   (define backend (make-project-keyed-backend id #:base-data-dir tmp))
   (check-not-false backend)
   ;; The backend's physical path should be under tmp and keyed by the identity.
   (define backend-path (project-keyed-backend-path backend))
   (check-true (string-contains? backend-path "key1"))
   (check-true (string-prefix? backend-path (path->string tmp)))
   (delete-directory/files tmp #:must-exist? #f))
 (test-case "make-project-keyed-backend returns #f for #f identity (fail-closed)"
   ;; No identity → no project-keyed backend. Caller must handle #f.
   (define tmp (make-temporary-file "q-factory2-~a" 'directory))
   (check-false (make-project-keyed-backend #f #:base-data-dir tmp))
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Isolation: two identities get separate storage ──
 (test-case "two identities get isolated storage"
   (define tmp (make-temporary-file "q-iso-~a" 'directory))
   (define id-a (make-test-identity "aaaa" "/some/a"))
   (define id-b (make-test-identity "bbbb" "/some/b"))
   (define be-a (make-project-keyed-backend id-a #:base-data-dir tmp))
   (define be-b (make-project-keyed-backend id-b #:base-data-dir tmp))
   (define path-a (project-keyed-backend-path be-a))
   (define path-b (project-keyed-backend-path be-b))
   (check-false (equal? path-a path-b))
   (check-false (string-contains? path-a "bbbb"))
   (check-false (string-contains? path-b "aaaa"))
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Same identity always maps to the same path (idempotent) ──
 (test-case "same identity maps to same path"
   (define tmp (make-temporary-file "q-idem-~a" 'directory))
   (define id (make-test-identity "feed" "/some/p"))
   (define be-1 (make-project-keyed-backend id #:base-data-dir tmp))
   (define be-2 (make-project-keyed-backend id #:base-data-dir tmp))
   (check-equal? (project-keyed-backend-path be-1) (project-keyed-backend-path be-2))
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Cross-session isolation: same project, fresh backend, same path ──
 (test-case "fresh same-project session resolves to same storage path"
   ;; Two identities for the SAME project (worktree alias): both should
   ;; map to the same physical memory path because same-project? holds.
   (define tmp (make-temporary-file "q-cross-~a" 'directory))
   (define main (make-test-identity "k1" "/some/main"))
   (define wt
     (make-project-identity #:key "k2"
                            #:root "/some/main-wt"
                            #:kind 'git-worktree
                            #:origin-url "https://github.com/u/r.git"
                            #:common-dir "/some/main/.git"))
   ;; Both should resolve to the MAIN repo's keyed path because they alias.
   (define r-main (project-keyed-memory-root main #:base-data-dir tmp))
   (define r-wt (project-keyed-memory-root/aliased wt main #:base-data-dir tmp))
   (check-equal? r-main r-wt)
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Different projects cannot see each other's storage ──
 (test-case "different projects have disjoint storage roots"
   (define tmp (make-temporary-file "q-disjoint-~a" 'directory))
   (define id-x (make-test-identity "xxxx" "/some/x" #:origin "https://github.com/u/x.git"))
   (define id-y (make-test-identity "yyyy" "/some/y" #:origin "https://github.com/u/y.git"))
   (define r-x (project-keyed-memory-root id-x #:base-data-dir tmp))
   (define r-y (project-keyed-memory-root id-y #:base-data-dir tmp))
   ;; Paths must differ AND neither contains the other's key.
   (check-false (equal? r-x r-y))
   (check-false (string-contains? r-x "yyyy"))
   (check-false (string-contains? r-y "xxxx"))
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── default base data dir resolution ──
 (test-case "default-base-data-dir is under user home"
   (define d (default-base-data-dir))
   (check-true (string? d))
   (check-true (> (string-length d) 0))
   ;; Must NOT be the session dir or cwd.
   (check-false (equal? d "."))
   (check-false (string-suffix? d "sessions"))))

(run-tests project-keyed-factory-suite)
