#lang racket
;; @speed fast
;; @suite extensions

(require rackunit
         racket/runtime-path
         "../extensions/gsd/effect-ports.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/system-adapters.rkt"
         "helpers/gsd-port-fakes.rkt")

(define-runtime-path effect-ports-source "../extensions/gsd/effect-ports.rkt")
(define-runtime-path facade-path "../extensions/gsd-planning.rkt")
(define-runtime-path core-path "../extensions/gsd/core.rkt")
(define-runtime-path repo-root "..")

(module+ test
  (test-case "default composition produces valid cohesive ports"
    (define ports system-gsd-effect-ports)
    (check-true (gsd-effect-ports? ports))
    (check-true (gsd-filesystem-port? (gsd-effect-ports-filesystem ports)))
    (check-true (gsd-git-port? (gsd-effect-ports-git ports)))
    (check-true (gsd-clock-port? (gsd-effect-ports-clock ports)))
    (check-true (gsd-process-port? (gsd-effect-ports-process ports)))
    (check-true (procedure? (gsd-effect-ports-event-sink ports))))

  (test-case "current composition supports dynamic DI and restores afterward"
    (define saved (current-gsd-effect-ports))
    (define-values (fake _state) (make-fake-gsd-effect-ports))
    (parameterize ([current-gsd-effect-ports fake])
      (check-eq? (current-gsd-effect-ports) fake))
    (check-eq? (current-gsd-effect-ports) saved))

  (test-case "two fake compositions do not share mutable state"
    (define-values (a a-state) (make-fake-gsd-effect-ports))
    (define-values (b b-state) (make-fake-gsd-effect-ports))
    ((gsd-effect-ports-event-sink a) 'gsd.wave.started #hasheq((wave . 0)))
    (check-equal? (length (fake-gsd-state-events a-state)) 1)
    (check-equal? (fake-gsd-state-events b-state) '())
    (check-not-eq? a b))

  (test-case "stable facades do not export internal ports or composition root"
    (for ([module-path (in-list (list facade-path core-path))]
          [name (in-cycle '(gsd-effect-ports current-gsd-effect-ports))])
      (check-exn exn:fail? (lambda () (dynamic-require module-path name)))))

  (test-case "system git adapter trims, truncates, and degrades on failure (via fake process)"
    (define (make-process returning)
      (gsd-process-port (lambda (_program _args _cwd) returning) (lambda () (void))))
    ;; trim
    (define trimmed
      ((gsd-git-port-head-summary
        (make-system-git-port (make-process (gsd-process-result 0 #"  abc123 change  \n" #""))))
       "/repo"
       '("a.rkt")))
    (check-equal? trimmed "abc123 change")
    ;; truncation at 2000 + "..."
    (define long-out (string->bytes/utf-8 (make-string 2100 #\x)))
    (define truncated
      ((gsd-git-port-head-summary
        (make-system-git-port (make-process (gsd-process-result 0 long-out #""))))
       "/repo"
       '("a.rkt")))
    (check-equal? (string-length truncated) 2003)
    (check-equal? (substring truncated 2000) "...")
    ;; empty file list → "" without invoking the process
    (check-equal?
     ((gsd-git-port-head-summary (make-system-git-port (make-process (gsd-process-result 0 #"" #""))))
      "/repo"
      '())
     "")
    ;; process failure → ""
    (check-equal? ((gsd-git-port-head-summary
                    (make-system-git-port (gsd-process-port (lambda (_p _a _c) (error 'boom))
                                                            (lambda () (void)))))
                   "/repo"
                   '("a.rkt"))
                  ""))

  (test-case "default git port runs against the real repository"
    (define result
      ((gsd-git-port-head-summary (gsd-effect-ports-git system-gsd-effect-ports))
       repo-root
       '("CHANGELOG.md" "README.md")))
    (check-true (and (string? result) (> (string-length result) 0))
                (format "real git head-summary produced: ~s" result)))

  (test-case "neutral contracts carry no concrete adapter dependencies"
    (define source (file->string effect-ports-source))
    (check-false (regexp-match?
                  #rx"require[^\n]*(?:runtime/|sandbox/|racket/file|racket/system|net/|github)"
                  source))))
