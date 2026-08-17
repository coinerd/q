#lang racket/base

;; extensions/gsd/delivery-verifier.rkt — Structured delivery verification
;; for the /go campaign coordinator.
;;
;; STABILITY: evolving
;;
;; Replaces the hardcoded fail-closed `#f` verifier in prepare-go-campaign.
;; A wave may only cross the durable DONE commit point when REAL delivery
;; evidence exists, not merely because the executor returned a normal
;; response. Evidence checks (all fail closed):
;;   1. git repository reachable from base-dir;
;;   2. current branch matches the wave's expected feature/issue-<N>-wave
;;      (issue number resolved from .planning/STATE.md wave table);
;;   3. at least one wave target file changed vs HEAD (or untracked-new);
;;   4. a bounded verify command exits 0 (compile gate by default).
;;
;; Returns a `delivery-verification` struct whose approved?/message the
;; coordinator surfaces on rejection. This is the only result that may
;; cross the durable DONE commit point.

(require racket/format
         racket/path
         racket/port
         racket/set
         racket/string
         racket/system
         "plan-types.rkt"
         "wave-docs.rkt"
         (only-in "plan-context-builder.rkt" find-git-root-dir))

(provide delivery-verification
         delivery-verification?
         delivery-verification-approved?
         delivery-verification-evidence
         delivery-verification-message
         make-delivery-verifier
         run-delivery-verification
         current-gsd-delivery-verify-command
         current-gsd-delivery-verify-timeout-sec
         check-git-available
         check-branch-matches
         check-wave-files-changed
         check-verify-command)

;; ============================================================
;; Structured verification result
;; ============================================================

(struct delivery-verification (approved? evidence message) #:transparent)
;; evidence: list of (cons check-name (cons ok? detail))

;; ============================================================
;; Policy knobs
;; ============================================================

;; Optional override for the verify command. When #f, a compile gate is
;; derived from the wave's target files. Used by tests to force failure.
(define current-gsd-delivery-verify-command
  (make-parameter
   #f
   (lambda (v)
     (cond
       [(not v) v]
       [(string? v) v]
       [else (raise-argument-error 'current-gsd-delivery-verify-command "(or/c #f string?)" v)]))))

;; Bounded runtime for the verify command (seconds).
(define current-gsd-delivery-verify-timeout-sec
  (make-parameter
   300
   (lambda (v)
     (if (and (real? v) (positive? v))
         v
         (raise-argument-error 'current-gsd-delivery-verify-timeout-sec "positive-real?" v)))))

;; ============================================================
;; Git helpers
;; ============================================================

(define (git-root-for base-dir)
  (find-git-root-dir base-dir))

(define (run-git* git-root args)
  ;; returns (list exit-code stdout) — exit-code #f on exception
  (define git (find-executable-path "git"))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define exit-code
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr])
        (if git
            (apply system*/exit-code git "-C" git-root args)
            #f))))
  (list exit-code (get-output-string stdout) (get-output-string stderr)))

(define (git-exit-ok? result)
  (and result (eq? (car result) 0)))

(define (git-stdout result)
  (if result
      (cadr result)
      ""))

(define (git-available? base-dir)
  (define root (git-root-for base-dir))
  (and root
       (directory-exists? root)
       (git-exit-ok? (run-git* root (list "rev-parse" "--is-inside-work-tree")))))

(define (current-branch git-root)
  (define result (run-git* git-root (list "rev-parse" "--abbrev-ref" "HEAD")))
  (and (git-exit-ok? result)
       (let ([b (string-trim (git-stdout result))]) (and (not (string=? b "")) b))))

;; ============================================================
;; Issue → expected branch resolution (from STATE.md wave table)
;; ============================================================

(define wave-table-rx #rx"^\\| *W([0-9]+) *\\| *#([0-9]+) *\\|")

(define (wave-issue-number base-dir wave-idx)
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (cond
    [(not (file-exists? state-path)) #f]
    [else
     (define text (call-with-input-file state-path port->string))
     (for/first ([line (in-list (string-split text "\n"))]
                 #:when (and (regexp-match wave-table-rx line)
                             (= (string->number (cadr (regexp-match wave-table-rx line))) wave-idx)))
       (define m (regexp-match wave-table-rx line))
       (cadr (cdr m)))]))

(define (expected-wave-branch base-dir wave-idx)
  (define issue (wave-issue-number base-dir wave-idx))
  (and issue (format "feature/issue-~a-wave" issue)))

;; ============================================================
;; Individual checks
;; ============================================================

(define (check-git-available base-dir)
  (cons "git"
        (if (git-available? base-dir)
            (cons #t "git repository reachable")
            (cons #f "no git repository reachable"))))

(define (check-branch-matches base-dir wave-idx)
  (define root (git-root-for base-dir))
  (define branch (and root (current-branch root)))
  (define expected (expected-wave-branch base-dir wave-idx))
  (define detail (format "branch=~a expected=~a" branch (or expected "?")))
  (cons "branch"
        (if (and branch expected (string=? branch expected))
            (cons #t detail)
            (cons #f detail))))

(define (wave-file->git-relative base-dir git-root wave-file)
  ;; Wave files are repo-root-relative (e.g. "q/ui-core/preferences.rkt").
  ;; Map to the path relative to git root (e.g. "ui-core/preferences.rkt"
  ;; when git root is <base>/q).
  (define abs (path->complete-path (build-path base-dir wave-file)))
  (define rel (find-relative-path git-root abs))
  (path->string rel))

(define (changed-files-set base-dir git-root)
  ;; returns a set of git-relative paths changed vs HEAD or untracked-new.
  (define diff-result (run-git* git-root (list "diff" "--name-only" "HEAD")))
  (define untracked-result (run-git* git-root (list "ls-files" "--others" "--exclude-standard")))
  (define paths
    (append (string-split (git-stdout diff-result) "\n")
            (string-split (git-stdout untracked-result) "\n")))
  (for/set ([p (in-list paths)]
            #:when (not (string=? (string-trim p) "")))
    (string-trim p)))

(define (check-wave-files-changed base-dir wave-idx plan)
  (define wave (and plan (plan-wave-ref plan wave-idx)))
  (define files
    (if wave
        (gsd-wave-files wave)
        '()))
  (define root (git-root-for base-dir))
  (cond
    [(not root) (cons "files" (cons #f "no git root"))]
    [(null? files) (cons "files" (cons #f "wave declares no target files"))]
    [else
     (let* ([changed (changed-files-set base-dir root)]
            [changed-wave-files
             (for/list ([f (in-list files)]
                        #:when (set-member? changed (wave-file->git-relative base-dir root f)))
               f)])
       (cons "files"
             (if (pair? changed-wave-files)
                 (cons #t (format "changed: ~a" (string-join changed-wave-files ", ")))
                 (cons #f (format "no wave target files changed: ~a" (string-join files ", "))))))]))

;; ============================================================
;; Verify command (bounded)
;; ============================================================

(define (build-compile-gate base-dir git-root wave-idx plan)
  ;; `raco make` on the wave's changed target files from the git root.
  (define wave (and plan (plan-wave-ref plan wave-idx)))
  (define files
    (if wave
        (gsd-wave-files wave)
        '()))
  (define changed (changed-files-set base-dir git-root))
  (define targets
    (for/list ([f (in-list files)]
               #:when (set-member? changed (wave-file->git-relative base-dir git-root f)))
      (wave-file->git-relative base-dir git-root f)))
  (if (null? targets)
      #f
      (string-join (cons "raco make" targets) " ")))

(define (run-verify-command command git-root timeout-sec)
  ;; Run a shell command bounded by timeout-sec from git-root.
  ;; Returns (list exit-code stdout) where exit-code is #f on timeout.
  (define-values (sp out in err)
    (parameterize ([current-directory git-root])
      (subprocess #f #f #f "/bin/sh" "-c" command)))
  (define deadline (+ (current-inexact-milliseconds) (* timeout-sec 1000.0)))
  (let loop ()
    (define status (subprocess-status sp))
    (cond
      [(eq? status 'running)
       (if (>= (current-inexact-milliseconds) deadline)
           (begin
             (subprocess-kill sp #t)
             (list #f "" ""))
           (begin
             (sleep 0.01)
             (loop)))]
      [else
       (subprocess-wait sp)
       (list status
             (if (and out (input-port? out))
                 (port->string out)
                 "")
             (if (and err (input-port? err))
                 (port->string err)
                 ""))])))

(define (check-verify-command base-dir wave-idx plan)
  (define root (git-root-for base-dir))
  (define command
    (or (current-gsd-delivery-verify-command)
        (and root (build-compile-gate base-dir root wave-idx plan))))
  (cond
    [(not root) (cons "verify" (cons #f "no git root"))]
    [(not command) (cons "verify" (cons #f "no verify command derivable"))]
    [else
     (let* ([result (run-verify-command command root (current-gsd-delivery-verify-timeout-sec))]
            [exit-code (car result)]
            [detail (format "cmd=~a exit=~a" command exit-code)])
       (cons "verify"
             (if (eq? exit-code 0)
                 (cons #t detail)
                 (cons #f detail))))]))

;; ============================================================
;; Composition
;; ============================================================

(define (run-delivery-verification base-dir plan wave-idx)
  ;; Run all evidence checks. A wave is approved only when every check passes.
  (define checks
    (list (check-git-available base-dir)
          (check-branch-matches base-dir wave-idx)
          (check-wave-files-changed base-dir wave-idx plan)
          (check-verify-command base-dir wave-idx plan)))
  (define failed
    (for/list ([c (in-list checks)]
               #:when (not (car (cdr c))))
      (format "~a: ~a" (car c) (cdr (cdr c)))))
  (if (null? failed)
      (delivery-verification #t checks "delivery verified")
      (delivery-verification #f
                             checks
                             (string-join (cons "delivery verification failed:" failed) "\n"))))

;; Verifier callback for the campaign coordinator: (lambda (wave-idx) ...)
;; returning a `delivery-verification` struct.
(define (make-delivery-verifier base-dir plan)
  (lambda (wave-idx) (run-delivery-verification base-dir plan wave-idx)))
