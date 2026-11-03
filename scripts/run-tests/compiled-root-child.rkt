#lang racket/base

;; scripts/run-tests/compiled-root-child.rkt — v1.00.32 W2 repair
;; (trial c01 window-2, run 41271542715)
;;
;; Root-on child-inheritance support for the test runner.
;;
;; The launcher (ci/prepared-environment/compiled-root.rkt `launch`)
;; exports Q_COMPILED_ROOT_FLAGS_FILE (plus the telemetry directory the
;; bootstrap reads) into the shard child. This module re-validates those
;; flags with the SAME shape rule the launcher applied and builds the argv
;; that injects the identical bootstrap into the runner's per-file racket
;; children:
;;
;;   rackunit files:
;;     racket <bootstrap…> -N raco -l- raco/main.rkt test -t <file>
;;     (the raco launcher binary is literally
;;      `exec racket -N raco -l- raco/main.rkt "$@"`; probe-verified
;;      byte-identical discovery output and exit codes)
;;
;;   other files:
;;     racket <bootstrap…> -N <file> -t- <file>
;;     (the unified bare-child entry, W1a round 5 / trial
;;     trial-2026-11-01-smoke-3 run 41303344486: the same mechanism as
;;     the proven raco dispatch `racket -N raco -l- raco/main.rkt …`,
;;     here in the file-path grammar because -l rejects absolute file
;;     paths. The former -e expression entry left find-system-path
;;     'run-file at the interpreter and could never route a command
;;     line; -t- loads the file as a module AND as the run-file — plain
;;     `racket <file>` semantics: module body, then the 'main submodule
;;     when declared, empty child argv)
;;
;; Without those test files' bootstrap the children inherit neither
;; current-compiled-file-roots nor the TOCTOU guard, so no test load could
;; ever be root-served and the launcher record could never report a real
;; hit (zero-root-hits).
;;
;; Fail-closed: when the env var is absent, the file unreadable, or the
;; flags fail validation, the prefix is #f and every invocation stays
;; byte-identical to the legacy path (the launcher has already selected
;; the one eager fallback in that situation). A malformed flags file is
;; reported loudly on stderr, never silently ignored.

(require racket/file
         racket/path
         racket/string)

(provide compiled-root-child-prefix
         compiled-root-raco-child-args
         compiled-root-bare-child-args)

;; The validated flags payload: (list 'compiled-root-flags root map
;; checkout (list launch-args…)) — same shape the launcher validated.
(define valid-flags?
  (lambda (v)
    (and (list? v)
         (= 5 (length v))
         (eq? (list-ref v 0) 'compiled-root-flags)
         (string? (list-ref v 1))
         (string? (list-ref v 2))
         (string? (list-ref v 3))
         (and (list? (list-ref v 4)) (andmap string? (list-ref v 4))))))

;; Cache keyed on the flags-file path: the runner spawns one child per
;; test file, so getenv+read must not repeat per child. The path key makes
;; the cache transparently recompute when the environment changes (tests).
(define prefix-cache (cons #f #f)) ; (cons key prefix|#f)

(define (compute-prefix)
  (define ff (getenv "Q_COMPILED_ROOT_FLAGS_FILE"))
  (cond
    [(not (and ff (non-empty-string? ff))) #f]
    [else
     (define flags
       (with-handlers ([exn:fail? (lambda (e)
                                    (eprintf "run-tests: compiled-root flags unreadable (~a): ~a\n"
                                             ff
                                             (exn-message e))
                                    #f)])
         (file->value (path->complete-path (string->path ff)))))
     (cond
       [(valid-flags? flags) (list-ref flags 4)]
       [else
        (eprintf "run-tests: compiled-root flags malformed (~a); running children unmapped\n" ff)
        #f])]))

;; compiled-root-child-prefix
;;   The bootstrap argv prefix (a list of racket command-line strings) for
;;   the runner's racket children, or #f when root-on child inheritance is
;;   not active.
(define (compiled-root-child-prefix)
  (define ff (getenv "Q_COMPILED_ROOT_FLAGS_FILE"))
  (define key (and ff (string? ff) ff))
  (if (equal? key (car prefix-cache))
      (cdr prefix-cache)
      (let ([p (compute-prefix)])
        (set! prefix-cache (cons key p))
        p)))

;; compiled-root-raco-child-args
;;   Full argv (after the racket binary) for a rackunit test file child
;;   under root-on, or #f to keep the legacy `raco test -t` invocation.
(define (compiled-root-raco-child-args resolved-path)
  (define prefix (compiled-root-child-prefix))
  (and prefix
       (append prefix
               (list "-N" "raco" "-l-" "raco/main.rkt" "test" "-t" (path->string resolved-path)))))

;; compiled-root-bare-child-args
;;   Full argv (after the racket binary) for a non-rackunit test file
;;   child under root-on, or #f to keep the legacy `racket <file>`
;;   invocation. The -N/-t- entry preserves `racket <file>` semantics
;;   exactly (body + declared 'main submodule, empty child argv) AND
;;   keeps the file as the run-file (the unified entry, see the
;;   launcher's compiled-root-script-entry-argument, W1a round 5).
(define (compiled-root-bare-child-args resolved-path)
  (define prefix (compiled-root-child-prefix))
  (and prefix (let ([p (path->string resolved-path)]) (append prefix (list "-N" p "-t-" p)))))
