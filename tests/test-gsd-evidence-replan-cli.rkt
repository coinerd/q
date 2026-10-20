#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @isolation temp-dir
;; @timeout 120
;; Real CLI subprocesses with isolated HOME and a temporary private store.
;; No live campaign, broker channel, authorization or publication is used.
(require rackunit
         json
         racket/file
         racket/port
         racket/runtime-path
         racket/system
         "../extensions/gsd/supplemental-evidence-authority.rkt")
(define-runtime-path cli "../scripts/gsd-evidence-replan.rkt")

(define (with-private-store proc)
  (define home (make-temporary-file "evidence-replan-cli-~a" 'directory))
  (define root (build-path home "private"))
  (dynamic-wind (lambda () (make-directory root #o700))
                (lambda ()
                  (define env (environment-variables-copy (current-environment-variables)))
                  (environment-variables-set! env #"HOME" (path->bytes home))
                  (parameterize ([current-environment-variables env]
                                 [current-supplemental-authority-root root])
                    (proc root)))
                (lambda () (delete-directory/files home))))

(define (run-cli root . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err]
                   ;; Text that looks like consent is deliberately NOT authority.
                   [current-input-port (open-input-string "yes\napproved: true\n")])
      (apply system*/exit-code
             (find-executable-path "racket")
             cli
             "--authority-root"
             (path->string root)
             args)))
  (values code (get-output-string out) (get-output-string err)))

(module+ test
  (test-case "inspect resolves a real retained proposal and accepts the digest argument"
    (with-private-store
     (lambda (root)
       (define ref (propose-retain! "task-proposal" (hasheq 'objective "CLI regression")))
       (define before (directory-list root))
       (define-values (code out err)
         (run-cli root "--verb" "inspect" "--kind" "task-proposal" "--sha256" (hash-ref ref 'sha256)))
       (check-equal? code 0 err)
       (check-equal? (string->jsexpr out) (load-supplemental-record ref))
       (check-equal? (directory-list root) before))))
  (test-case "malformed digest is a typed CLI refusal, not a missing argument"
    (with-private-store
     (lambda (root)
       (define-values (code out err)
         (run-cli root "--verb" "inspect" "--kind" "task-proposal" "--sha256" "bad"))
       (check-equal? code 2)
       (check-equal? out "")
       (check-regexp-match #rx"--sha256 must be a 64-hex digest" err)
       (check-equal? (directory-list root) '()))))
  (test-case "batch authorization parses plan and wave but cannot turn stdin into approval"
    (with-private-store (lambda (root)
                          (define-values (code out err)
                            (run-cli root
                                     "--verb"
                                     "authorize"
                                     "--campaign-root"
                                     (path->string root)
                                     "--plan"
                                     (make-string 64 #\a)
                                     "--wave"
                                     "1"
                                     "--stage"
                                     "task"))
                          (check-equal? code 2)
                          (check-equal? out "")
                          (check-regexp-match #rx"no active interactive approval channel" err)
                          (check-equal? (directory-list root) '())))))
