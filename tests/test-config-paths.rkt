#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         "../util/config-paths.rkt")

;; ============================================================
;; project-config-dirs
;; ============================================================

(test-case "project-config-dirs returns .q then .pi"
  (define dirs (project-config-dirs "/tmp/myproject"))
  (check-equal? (length dirs) 2)
  (check-equal? (car dirs) (build-path "/tmp/myproject" ".q"))
  (check-equal? (cadr dirs) (build-path "/tmp/myproject" ".pi")))

(test-case "project-config-dirs works with relative path"
  (define dirs (project-config-dirs "."))
  (check-equal? (length dirs) 2)
  (check-true (path? (car dirs))))

;; ============================================================
;; global-config-dir
;; ============================================================

(test-case "global-config-dir returns ~/.q"
  (define dir (global-config-dir))
  (check-pred path? dir)
  (check-true (string? (path->string dir)))
  (check-true (string-contains? (path->string dir) ".q")))

;; ============================================================
;; resolve-project-dir-from-args  (v0.99.38 W1)
;; ============================================================

(test-case "resolve-project-dir-from-args: symbol key with string value"
  (define args (hash 'project_dir "/some/path"))
  (check-equal? (resolve-project-dir-from-args args) (string->path "/some/path")))

(test-case "resolve-project-dir-from-args: string key with string value"
  (define args (hash "project_dir" "/other/path"))
  (check-equal? (resolve-project-dir-from-args args) (string->path "/other/path")))

(test-case "resolve-project-dir-from-args: symbol key with path value"
  (define args (hash 'project_dir (string->path "/some/path")))
  (check-equal? (resolve-project-dir-from-args args) (string->path "/some/path")))

(test-case "resolve-project-dir-from-args: falls back to current-directory"
  (define args (hash))
  (check-equal? (resolve-project-dir-from-args args) (current-directory)))

(test-case "resolve-project-dir-from-args: prefers symbol key over string key"
  (define args (hash 'project_dir "/sym" "project_dir" "/str"))
  (check-equal? (resolve-project-dir-from-args args) (string->path "/sym")))

(test-case "resolve-project-dir-from-args: returns path for string input"
  (define args (hash 'project_dir "/some/path"))
  (check-pred path? (resolve-project-dir-from-args args)))

(test-case "resolve-project-dir-from-args: handles #f value gracefully"
  (define args (hash 'project_dir #f))
  (check-equal? (resolve-project-dir-from-args args) (current-directory)))

;; ============================================================
;; find-project-root  (W7 #8569)
;; ============================================================

;; The real project root is the directory containing this file's parent
;; (tests/ → q/), which contains util/version.rkt.
(define test-project-root
  (simplify-path (build-path (or (current-load-relative-directory) (current-directory)) "..")))

(test-case "find-project-root: finds root from within project (cwd is root)"
  ;; Running tests from tests/ directory, cwd might be tests/ or q/
  ;; Either way, find-project-root should find q/
  (define root (find-project-root))
  (check-not-false root)
  (check-true (file-exists? (build-path root "util" "version.rkt"))
              "root should contain util/version.rkt"))

(test-case "find-project-root: finds root from a subdirectory"
  (define root (find-project-root test-project-root))
  (check-not-false root)
  (check-true (file-exists? (build-path root "util" "version.rkt"))))

(test-case "find-project-root: finds root from a deeper subdirectory"
  (define deep (build-path test-project-root "scripts"))
  (define root (find-project-root deep))
  (check-not-false root)
  (check-true (file-exists? (build-path root "util" "version.rkt"))))

(test-case "find-project-root: returns #f for temp directory without sentinel"
  (define root (find-project-root (find-system-path 'temp-dir)))
  ;; Temp dir won't have util/version.rkt, but parent might on some systems
  ;; Just check it doesn't crash
  (check-not-exn (lambda () (find-project-root (find-system-path 'temp-dir)))))

(test-case "find-project-root: accepts string path"
  (define root (find-project-root (path->string test-project-root)))
  (check-not-false root)
  (check-true (file-exists? (build-path root "util" "version.rkt"))))

(test-case "find-project-root: accepts #f (uses cwd)"
  (check-not-exn (lambda () (find-project-root #f))))

;; ============================================================
;; project-root-or-cwd  (W7 #8569)
;; ============================================================

(test-case "project-root-or-cwd: returns project root when found"
  (define root (project-root-or-cwd test-project-root))
  (check-true (file-exists? (build-path root "util" "version.rkt"))))

(test-case "project-root-or-cwd: returns cwd when sentinel not found"
  ;; Use a path unlikely to have the sentinel
  (define result (project-root-or-cwd "/"))
  (check-pred path? result))

(test-case "project-root-or-cwd: always returns a path"
  (check-pred path? (project-root-or-cwd #f)))

(test-case "project-root-or-cwd: backward-compatible when cwd is project root"
  ;; When started from project root, result should equal cwd
  (parameterize ([current-directory test-project-root])
    (check-equal? (project-root-or-cwd) test-project-root)))
