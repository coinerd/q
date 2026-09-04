#lang racket

;; @speed fast
;; @suite default
;; @boundary unit
;; RA-4 W1: the BUG-0033 spot-checks no longer spawn nested full test runs.
;; A single minimal probe (tests/fixtures/cwd-invocation-probe.rkt) runs in
;; a subprocess from an isolated private temp directory; the real audit
;; CWD behavior is owned by tests/test-audit-script.rkt in slow/L4.

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

;; ============================================================
;; Test 7: RA-4 W1 — CWD-independence probe invocable from an
;; arbitrary cwd without nested suite execution.
;; The probe is launched by absolute path while current-directory is an
;; isolated private temp directory. All subprocess-induced writes are
;; kept inside that private directory (PLT_COMPILED_DIR is redirected
;; there); the test asserts the exit code, the deterministic sentinel,
;; and that no writes landed outside the private temp directory.
;; ============================================================

(require racket/port
         racket/system)

(define racket-exe (find-executable-path "racket"))
(define probe-path
  (simplify-path (build-path this-module-dir "fixtures" "cwd-invocation-probe.rkt")))

;; Runs the probe with cwd = <private>/cwd, private output dir
;; <private>/probe-out, and PLT_COMPILED_DIR = <private>.
;; Returns (values exit-code stdout out-dir-entries cwd-dir-entries).
(define (run-probe-in-private-dir! private-dir)
  (define out-dir (build-path private-dir "probe-out"))
  (define cwd-dir (build-path private-dir "cwd"))
  (make-directory out-dir)
  (make-directory cwd-dir)
  (define stdout-path (build-path private-dir "probe-stdout.txt"))
  (define env
    (make-environment-variables
     #"PLT_COMPILED_DIR" (string->bytes/utf-8 (path->string private-dir))))
  (define exit-code
    (parameterize ([current-directory cwd-dir]
                   [current-environment-variables env])
      (define stdout-file
        (open-output-file stdout-path #:exists 'truncate/replace))
      (define-values (proc _stdout _stdin _stderr)
        (subprocess stdout-file #f stdout-file
                    racket-exe (path->string probe-path) (path->string out-dir)))
      (close-output-port stdout-file)
      (subprocess-wait proc)
      (subprocess-status proc)))
  (define stdout (file->string stdout-path))
  (values exit-code
          stdout
          (map path->string (directory-list out-dir))
          (map path->string (directory-list cwd-dir))))

(test-case "RA-4 W1: minimal CWD probe passes from an isolated arbitrary cwd"
  (define private-dir (make-temporary-file "cwd-probe-~a" 'directory))
  (dynamic-wind
   void
   (λ ()
     (define-values (exit-code stdout out-entries cwd-entries)
       (run-probe-in-private-dir! private-dir))
     (check-equal? exit-code 0 "probe must exit 0 from an arbitrary cwd")
     (check-true (string-prefix? stdout "cwd-invocation-probe:sentinel")
                 "probe must print the deterministic sentinel")
     (check-true (string-contains? stdout "registry-defaults.rkt")
                 "sentinel must name the required runtime-path target")
     (check-equal? out-entries '("probe-sentinel.txt")
                   "probe must write only its sentinel file")
     (check-equal? cwd-entries '()
                   "probe must not write outside its private temp directory"))
   (λ () (delete-directory/files private-dir))))
