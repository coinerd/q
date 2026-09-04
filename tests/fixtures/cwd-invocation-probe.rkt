#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration
;; @not-test
;;
;; tests/fixtures/cwd-invocation-probe.rkt — minimal CWD-independence probe
;; (RA-4 W1). Purpose-built replacement for the nested full-test spot-checks:
;; this probe must never crawl the repository, never spawn a nested suite,
;; and never write outside the caller-provided private output directory.
;;
;; Contract exercised by tests/test-cwd-independence.rkt:
;;   1. Resolves its own module path at runtime (no assumption about the
;;      invoking process's working directory).
;;   2. Requires one representative runtime-path target, the F-11
;;      CWD-independent module agent/registry-defaults.rkt, resolved by
;;      runtime path arithmetic from the probe's own source location.
;;   3. Writes a deterministic sentinel file into the single directory passed
;;      as argv[1] and prints the sentinel payload.
;;   4. Fails loudly (nonzero exit, no sentinel) on any missing precondition.

(require racket/file
         racket/path
         racket/string)

;; Self-path resolution: same pattern as agent/registry-defaults.rkt (F-11),
;; including the embedded-module fallback for raco-exe packaging.
(define this-module-dir
  (with-handlers ([exn:fail? (λ (_) #f)])
    (let* ([vr (#%variable-reference)]
           [resolved (variable-reference->resolved-module-path vr)]
           [path (resolved-module-path-name resolved)])
      (if (path? path)
          (let-values ([(dir _name _dir?) (split-path path)]) dir)
          #f))))

(module+ main
  (define args (current-command-line-arguments))
  (when (not (= 1 (vector-length args)))
    (raise-user-error 'cwd-invocation-probe
                      "expected exactly one argument: the private output directory"))
  (define out-dir (simplify-path (string->path (vector-ref args 0))))
  (when (not (directory-exists? out-dir))
    (raise-user-error 'cwd-invocation-probe
                      "output directory does not exist: ~a" (path->string out-dir)))
  (when (not (path? this-module-dir))
    (raise-user-error 'cwd-invocation-probe
                      "failed to resolve probe's own module directory"))
  ;; One representative runtime-path target: the canonical F-11
  ;; CWD-independent module, resolved from the probe's own location.
  (define target-path
    (simplify-path (build-path this-module-dir ".." ".." "agent" "registry-defaults.rkt")))
  (when (not (file-exists? target-path))
    (raise-user-error 'cwd-invocation-probe
                      "runtime-path target failed to resolve from cwd ~a"
                      (path->string (current-directory))))
  (dynamic-require target-path #f)
  (define payload
    (string-append
     "cwd-invocation-probe:sentinel"
     ":runtime-path=" (path->string target-path)
     ":module=" (path->string this-module-dir)))
  (define sentinel-path (build-path out-dir "probe-sentinel.txt"))
  (display-to-file payload sentinel-path #:exists 'truncate/replace)
  (displayln (string-replace payload "\n" " ")))
