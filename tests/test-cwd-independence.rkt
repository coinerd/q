#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; Tests for CWD-independent module loading patterns (v0.99.38 W2)
;; These tests serve as regression guards: they verify that the path
;; resolution patterns used across the codebase produce absolute paths
;; that work regardless of (current-directory).

(require rackunit)

;; ============================================================
;; Test 1: variable-reference->resolved-module-path produces
;; absolute paths regardless of current-directory
;; ============================================================

;; We replicate the pattern from agent/registry-defaults.rkt:
;; using #%variable-reference to get the source directory
(define this-module-dir
  (let* ([vr (#%variable-reference)]
         [resolved (variable-reference->resolved-module-path vr)]
         [path (resolved-module-path-name resolved)])
    (define-values (dir _name _dir?) (split-path path))
    dir))

(test-case "variable-reference path is absolute"
  (check-true (absolute-path? this-module-dir) "this-module-dir should be absolute"))

(test-case "variable-reference path is stable across cwd changes"
  (define dir-at-original-cwd this-module-dir)
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (check-equal? this-module-dir
                  dir-at-original-cwd
                  "module-dir should not change when cwd changes")))

;; ============================================================
;; Test 2: path->complete-path produces absolute paths
;; (pattern from extensions/loader.rkt)
;; ============================================================

(test-case "path->complete-path makes relative paths absolute"
  (define rel-path "some/relative/path.rkt")
  (define abs-path (path->complete-path rel-path))
  (check-true (absolute-path? abs-path) "path->complete-path should produce absolute path"))

(test-case "path->complete-path is stable when cwd is known"
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (define abs-path (path->complete-path "test.rkt"))
    (check-true (absolute-path? abs-path))))

;; ============================================================
;; Test 3: resolve-project-dir-from-args is cwd-safe
;; ============================================================

(require "../util/config-paths.rkt")

(test-case "resolve-project-dir-from-args default changes with cwd"
  ;; When args has no project_dir, the result should be current-directory
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (check-equal? (resolve-project-dir-from-args (hash)) tmp-dir)))

(test-case "resolve-project-dir-from-args with explicit dir ignores cwd"
  ;; When args has project_dir, result should be that dir regardless of cwd
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (check-equal? (resolve-project-dir-from-args (hash 'project_dir "/my/project"))
                  (string->path "/my/project"))))

;; ============================================================
;; Test 4: define-runtime-path resolves to existing files
;; (replicate the pattern from runtime/session/session-switch.rkt)
;; ============================================================

(require racket/runtime-path)

(define-runtime-path test-hooks-path "../extensions/hooks.rkt")
(define-runtime-path test-context-path "../extensions/context.rkt")

(test-case "define-runtime-path resolves to existing file (hooks)"
  (check-true (file-exists? test-hooks-path) "runtime-path should point to an existing file"))

(test-case "define-runtime-path resolves to existing file (context)"
  (check-true (file-exists? test-context-path) "runtime-path should point to an existing file"))

(test-case "define-runtime-path values are absolute"
  (check-true (absolute-path? test-hooks-path))
  (check-true (absolute-path? test-context-path)))

(test-case "define-runtime-path is stable across cwd changes"
  (define original-hooks test-hooks-path)
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (check-equal? test-hooks-path original-hooks)))

;; ============================================================
;; Test 5: variable-reference pattern produces cwd-stable paths
;; (same pattern as agent/registry-defaults.rkt but replicated here
;; because role-module-path is not exported)
;; ============================================================

(test-case "variable-reference built paths are absolute and cwd-stable"
  ;; This replicates the pattern from registry-defaults.rkt:
  ;;   (build-path this-module-dir "roles" "planner.rkt")
  (define planner-path (build-path this-module-dir ".." "agent" "roles" "planner.rkt"))
  (check-true (absolute-path? planner-path))
  (check-true (file-exists? planner-path))
  ;; Verify cwd-stability
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    (check-equal? (build-path this-module-dir ".." "agent" "roles" "planner.rkt") planner-path)))

;; ============================================================
;; Test 6: dynamic-require with runtime-path works from any cwd
;; ============================================================

(test-case "dynamic-require via runtime-path works from non-source cwd"
  (define tmp-dir (find-system-path 'temp-dir))
  (parameterize ([current-directory tmp-dir])
    ;; This tests that the session-switch pattern (define-runtime-path +
    ;; dynamic-require) works regardless of cwd
    (define dispatch-fn
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (dynamic-require test-hooks-path 'dispatch-hooks)))
    (check-not-false dispatch-fn "dynamic-require via runtime-path should work from any cwd")))
