#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/runtime-path)

(define-runtime-path q-root "..")
(define approval-modules
  '("../runtime/approval/broker.rkt" "../tui/approval-channel.rkt"
                                     "../tools/builtins/spawn-approval.rkt"))
(define forbidden-exports
  '(approval-await-result approval-put!
                          approval-channel-ch
                          register-approval-request!
                          current-spawn-approval-result
                          approval-consume-grant!
                          set-headless-approval-mode!
                          headless-approval-mode?))
(define frontend-forbidden-exports
  '(register-approval-request-for-channel! approval-await-grant
                                           approval-grant?
                                           approval-consume-grant!
                                           call-with-approval-grant
                                           cancel-approval-request!
                                           clear-pending-approvals!
                                           pending-approval-count))
(define (production-racket-files)
  (for/list ([path (in-directory q-root)]
             #:when (regexp-match? #px"[.]rkt$" (path->string path))
             #:unless (regexp-match? #px"/(tests|docs|compiled|[.]git)/" (path->string path)))
    path))

(define suite
  (test-suite "legacy approval API removal"

    (test-case "legacy no-ID and mutable headless authority names are not exported"
      (for* ([module-path (in-list approval-modules)]
             [name (in-list forbidden-exports)])
        (check-exn exn:fail?
                   (lambda () (dynamic-require module-path name))
                   (format "~a must not export ~a" module-path name))))

    (test-case "TUI adapter exposes no runtime registration or grant authority"
      (for ([name (in-list frontend-forbidden-exports)])
        (check-exn exn:fail?
                   (lambda () (dynamic-require "../tui/approval-channel.rkt" name))
                   (format "TUI adapter must not export ~a" name))))

    (test-case "production contains no legacy calls or mutable headless authority"
      (define forbidden
        (pregexp
         (string-append
          "(approval-await-result|approval-put!|approval-channel-ch|register-approval-request!|"
          "current-spawn-approval-result|approval-consume-grant!|"
          "set-headless-approval-mode!|headless-approval-mode[?])")))
      (define offenders
        (for/list ([path (in-list (production-racket-files))]
                   #:when (regexp-match? forbidden (file->string path)))
          (path->string path)))
      (check-equal? offenders '()))))

(exit (run-tests suite))
