#!/usr/bin/env racket
#lang racket/base

;; scripts/ci-package-setup.rkt — Local CI-equivalent package compile gate.
;;
;; Reproduces the hidden GitHub Actions setup-racket compile boundary
;; that runs `raco pkg install` (or `raco pkg update`) which compiles
;; ALL package-visible modules, not just main.rkt and its transitive deps.
;;
;; Two modes:
;;   --preflight   (Option B) Compile all package-visible .rkt files via raco make.
;;                 Fast, deterministic, no package state mutation.
;;   --full        (Option A) Run true raco pkg install in isolated PLTUSERHOME.
;;                 Slowest, closest to CI, may download packages.
;;
;; Usage:
;;   cd q/ && racket scripts/ci-package-setup.rkt --preflight
;;   cd q/ && racket scripts/ci-package-setup.rkt --full
;;
;; Default (no flag): runs --preflight.
;; Exit 0 if all modules compile, 1 if any fail.

(require racket/list
         racket/string
         racket/system
         racket/match
         racket/file)

;; ---------------------------------------------------------------------------
;; Provide pure functions for testing
;; ---------------------------------------------------------------------------

(provide build-package-setup-command
         classify-failure
         find-package-visible-modules
         run-preflight-compile
         run-full-package-setup
         main)

;; ---------------------------------------------------------------------------
;; Pure: Build the PLTUSERHOME-isolated package setup command
;; ---------------------------------------------------------------------------

(define (build-package-setup-command project-dir #:plt-user-home [plt-user-home #f])
  "Build the raco pkg install command with optional PLTUSERHOME isolation.
Returns a string suitable for system/exit-code."
  (define home (or plt-user-home "/tmp/plt-user-home-ci"))
  (format "PLTUSERHOME=~a raco pkg install --auto --batch ~a" home project-dir))

;; ---------------------------------------------------------------------------
;; Pure: Classify failure output
;; ---------------------------------------------------------------------------

(define (classify-failure stderr-output)
  "Classify a package setup failure into a category string.
Returns one of:
  'dependency-install   — raco could not resolve/install a dependency
  'compile-failure      — a module failed to compile
  'cache-path           — cache or path issue
  'unknown              — unclassifiable"
  (cond
    [(string-contains? stderr-output "cannot find package") 'dependency-install]
    [(string-contains? stderr-output "unbound identifier") 'compile-failure]
    [(string-contains? stderr-output "identifier already") 'compile-failure]
    [(string-contains? stderr-output "cannot open module file") 'compile-failure]
    [(string-contains? stderr-output "cache") 'cache-path]
    [(string-contains? stderr-output "no such file or directory") 'cache-path]
    [else 'unknown]))

;; ---------------------------------------------------------------------------
;; Pure: Find package-visible .rkt modules
;; ---------------------------------------------------------------------------

(define (find-package-visible-modules project-dir)
  "Find all .rkt files that are visible to raco pkg setup.
Excludes tests/ (which have their own test runner), and compiled/."
  (define all-rkt
    (for/list ([f (in-directory project-dir)]
               #:when (and (regexp-match? #rx"\\.rkt$" (path->string f))
                           ;; Exclude compiled/ directories
                           (not (string-contains? (path->string f) "/compiled/"))
                           ;; Exclude tests/ (covered by run-tests.rkt)
                           (not (string-contains? (path->string f) "/tests/"))
                           ;; Exclude .planning/
                           (not (string-contains? (path->string f) "/.planning/"))
                           ;; Exclude tmp files
                           (not (regexp-match? #rx"/tmp-" (path->string f)))))
      (path->string f)))
  (sort all-rkt string<?))

;; ---------------------------------------------------------------------------
;; Option B: Preflight compile (fast, deterministic)
;; ---------------------------------------------------------------------------

(define (run-preflight-compile project-dir)
  "Compile all package-visible modules via raco make.
Returns (values success? failure-details-list)."
  (define modules (find-package-visible-modules project-dir))
  (printf "Package-visible modules: ~a~n" (length modules))
  (printf "Running raco make on each module...~n~n")
  (define failures
    (for/list ([mod (in-list modules)]
               #:when (not (compile-single-module mod)))
      mod))
  (if (null? failures)
      (begin
        (printf "~n✅ All ~a package-visible modules compile.~n" (length modules))
        (values #t '()))
      (begin
        (printf "~n❌ ~a/~a modules failed to compile.~n" (length failures) (length modules))
        (values #f failures))))

(define (compile-single-module module-path)
  "Compile a single module via raco make. Returns #t on success, #f on failure."
  (define cmd (format "raco make ~a 2>&1" module-path))
  (define exit-code (system/exit-code cmd))
  (zero? exit-code))

;; ---------------------------------------------------------------------------
;; Option A: Full package setup (slow, true CI parity)
;; ---------------------------------------------------------------------------

(define (run-full-package-setup project-dir)
  "Run raco pkg install in an isolated PLTUSERHOME.
Returns (values success? output-string)."
  (define tmp-home (path->string (make-temporary-file "plt-user-home-ci-~a" 'directory)))
  (printf "Using isolated PLTUSERHOME: ~a~n" tmp-home)
  (define cmd (build-package-setup-command project-dir #:plt-user-home tmp-home))
  (printf "Running: ~a~n~n" cmd)
  (define out (open-output-string))
  (define exit-code
    (parameterize ([current-output-port out]
                   [current-error-port out])
      (system/exit-code cmd)))
  (define output (get-output-string out))
  ;; Cleanup
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-home))
  (values (zero? exit-code) output))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (define mode
    (cond
      [(member "--full" args) 'full]
      [(member "--preflight" args) 'preflight]
      [else 'preflight])) ; default to preflight

  ;; Ensure we're in the q/ directory
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (printf "=== CI Package Setup Gate ===~n")
  (printf "Mode: ~a~n~n" mode)

  (define project-dir (current-directory))

  (case mode
    [(preflight)
     (define-values (success? failures) (run-preflight-compile project-dir))
     (if success?
         (exit 0)
         (begin
           (printf "~nFailed modules:~n")
           (for ([f (in-list failures)])
             (printf "  ✗ ~a~n" f))
           (exit 1)))]
    [(full)
     (define-values (success? output) (run-full-package-setup project-dir))
     (if success?
         (begin
           (printf "✅ Package setup succeeded.~n")
           (exit 0))
         (let* ([category (classify-failure output)]
                [lines (string-split output "\n")]
                [tail (take-right lines (min 20 (length lines)))])
           (printf "❌ Package setup failed.~n")
           (printf "Failure category: ~a~n" category)
           (printf "~n--- Output (last 20 lines) ---~n")
           (for ([l (in-list tail)])
             (printf "  ~a~n" l))
           (exit 1)))]))

(module+ main
  (main))
