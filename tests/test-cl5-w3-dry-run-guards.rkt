#lang racket

;; Guard tests for scripts/cl5/w3-dry-run.rkt (CL5 W3.1 dry run).
;; These spawn real git in throwaway repositories: the preflight and
;; output-confinement refusals cannot be exercised by pure functions.
;; No network, no model, no ./qt invocation.

;; @suite default
;; @speed fast
;; @boundary integration

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "../scripts/cl5/selection-cases.rkt"
         "../scripts/cl5/w3-dry-run.rkt")

(define git (find-executable-path "git"))

(define (git! dir . args)
  (define-values (process out-port in-port err-port)
    (apply subprocess #f #f #f (append (list git "-C" dir) args)))
  (define out (port->string out-port))
  (define err (port->string err-port))
  (subprocess-wait process)
  (unless (zero? (subprocess-status process))
    (raise-arguments-error 'git! "git command failed" "args" args "stderr" err))
  (string-trim out))

(define (make-repo)
  (define dir (path->string (make-temporary-file "cl5-w3-guards-~a" 'directory)))
  (git! dir "init" "-q")
  (call-with-output-file (build-path dir "main.rkt")
                         (lambda (out)
                           (display "#lang racket/base\n(provide x)\n(define x 1)\n" out)))
  (git! dir "add" "-A")
  (git! dir "-c" "user.name=test" "-c" "user.email=test@example.invalid" "commit" "-q" "-m" "init")
  (values dir (git! dir "rev-parse" "HEAD")))

(define (with-repo thunk)
  (define-values (dir sha) (make-repo))
  (dynamic-wind void
                (lambda () (thunk dir sha))
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (_) (void))])
                    (git! dir "worktree" "prune"))
                  (delete-directory/files dir))))

(define empty-cache (hash))

(test-case "an output path inside the measurement worktree is refused"
  (with-repo (lambda (dir sha)
               (check-exn exn:fail?
                          (lambda ()
                            (run-dry-case dir
                                          sha
                                          (selection-case 'D5)
                                          #:ci-cache empty-cache
                                          #:out-file (build-path dir "record.json")))))))

(test-case "a primary checkout is refused as a measurement worktree"
  (with-repo (lambda (dir sha)
               (check-exn exn:fail?
                          (lambda ()
                            (run-dry-case dir
                                          sha
                                          (selection-case 'D5)
                                          #:ci-cache empty-cache
                                          #:out-file "/tmp/cl5-w3-guard-primary.json"))))))

(test-case "a worktree whose HEAD is not the pinned revision is refused"
  (with-repo (lambda (dir sha)
               (define linked (path->string (make-temporary-file "cl5-w3-guards-wt-~a" 'directory)))
               (delete-directory linked)
               (git! dir "worktree" "add" "--detach" linked sha)
               (check-exn exn:fail?
                          (lambda ()
                            (run-dry-case linked
                                          (string-append (substring sha 0 39) "0")
                                          (selection-case 'D5)
                                          #:ci-cache empty-cache
                                          #:out-file "/tmp/cl5-w3-guard-rev.json")))
               (git! dir "worktree" "remove" "--force" linked))))

(module+ test
  (unless git
    (printf "git not found; guard tests skipped\n")))
