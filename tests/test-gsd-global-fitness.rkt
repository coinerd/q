#lang racket

;; tests/test-gsd-global-fitness.rkt
;; W9: No-new-GSD-globals fitness gate
;;
;; Static scan that fails if new production code uses gsd-default-ctx,
;; gsd-event-bus-box, or direct global GSD mutation outside the
;; designated compatibility modules.

(require rackunit
         rackunit/text-ui
         racket/path
         racket/port
         racket/runtime-path
         racket/string)

;; Allowed modules where gsd-default-ctx and gsd-event-bus-box may be referenced.
;; These are the compatibility/shim layer — all new code should use ctx-aware APIs.
(define allowed-for-gsd-default-ctx
  '("extensions/gsd/session-state.rkt" "extensions/gsd/state-machine.rkt"
                                       "extensions/gsd/events.rkt"))

(define allowed-for-gsd-event-bus-box '("extensions/gsd/events.rkt" "extensions/gsd-planning.rkt"))

;; Scan all .rkt files under the source tree (excluding tests).
;; Resolve relative to this test file so results are stable under both
;; `racket tests/...` and `raco test tests/...`.
(define-runtime-path source-root "../")

(define (find-production-rkt-files)
  (define output
    (with-output-to-string (lambda ()
                             (system* (find-executable-path "find")
                                      source-root
                                      "-name"
                                      "*.rkt"
                                      "-not"
                                      "-path"
                                      "*/tests/*"
                                      "-not"
                                      "-path"
                                      "*/compiled/*"
                                      "-type"
                                      "f"))))
  (filter (lambda (p) (string-suffix? p ".rkt"))
          (map string-trim (string-split output "\n" #:trim? #f))))

(define (relative-path full-path)
  (path->string (find-relative-path source-root (simplify-path full-path))))

(define (file-contains? path needle)
  (call-with-input-file path
                        (lambda (in)
                          (for/or ([line (in-lines in)])
                            (string-contains? line needle)))))

(define gsd-global-fitness-suite
  (test-suite "gsd-global-fitness-gate"

    (test-case "gsd-default-ctx only in allowed modules"
      (define files (find-production-rkt-files))
      (define violators
        (for/list ([f (in-list files)]
                   #:when (file-contains? f "gsd-default-ctx")
                   #:unless (member (relative-path f) allowed-for-gsd-default-ctx))
          (relative-path f)))
      (check-equal? violators
                    '()
                    (format "gsd-default-ctx found outside allowed modules: ~a"
                            (string-join violators ", "))))

    (test-case "gsd-event-bus-box only in allowed modules"
      (define files (find-production-rkt-files))
      (define violators
        (for/list ([f (in-list files)]
                   #:when (file-contains? f "gsd-event-bus-box")
                   #:unless (member (relative-path f) allowed-for-gsd-event-bus-box))
          (relative-path f)))
      (check-equal? violators
                    '()
                    (format "gsd-event-bus-box found outside allowed modules: ~a"
                            (string-join violators ", "))))

    (test-case "no new with-gsd-lock usage outside session-state.rkt"
      (define allowed-for-with-gsd-lock
        '("extensions/gsd/session-state.rkt" "extensions/gsd/state-machine.rkt"
                                             "extensions/gsd/command-handlers.rkt"))
      (define files (find-production-rkt-files))
      (define violators
        (for/list ([f (in-list files)]
                   #:when (file-contains? f "with-gsd-lock")
                   #:unless (member (relative-path f) allowed-for-with-gsd-lock))
          (relative-path f)))
      (check-equal? violators
                    '()
                    (format "with-gsd-lock found outside allowed modules: ~a"
                            (string-join violators ", "))))

    (test-case "allowed module baseline is non-empty"
      (check-true (pair? allowed-for-gsd-default-ctx) "baseline exists")
      (check-true (pair? allowed-for-gsd-event-bus-box) "baseline exists"))))

(run-tests gsd-global-fitness-suite)
