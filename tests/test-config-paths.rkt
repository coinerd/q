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
