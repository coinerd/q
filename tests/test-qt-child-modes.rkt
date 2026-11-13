#lang racket
;; @speed slow
;; @suite testing
;; @boundary integration
;; @isolation process
;; @timeout 300

;; Contract tests for the quick execution substrate
;; (scripts/testing/quick-child.rkt and scripts/testing/quick-runner.rkt):
;;
;;   1. A file whose only entry point is a `main` submodule runs and reports its
;;      RackUnit counts (the repository's script convention).
;;   2. A file declaring both `test` and `main` runs exactly one entry point
;;      (test wins), so no assertion is executed or counted twice.
;;   3. A file declaring neither still runs its module body.
;;   4. A per-file timeout overrides the run default (a slow file is allowed
;;      more than the default) and a short per-file timeout surfaces as reason
;;      'timeout.
;;   5. A non-zero child exit after valid evidence keeps the real exit code and
;;      invalidates the result with reason 'exit.
;;   6. Relative input spellings normalize to one identity, and duplicate
;;      normalized inputs are still rejected.
;;   7. `#:timeouts` is validated before any child starts.

(require rackunit
         racket/file
         "../scripts/testing/quick-runner.rkt"
         (submod "../scripts/testing/quick-runner.rkt" test-support)
         "../sandbox/subprocess.rkt")

(define (quick-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (find-executable-path "setsid")))

(define (write-fixture! dir name source)
  (define path (build-path dir name))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display source out)))
  (path->string path))

;; (1) main-only: the entry point is a `main` submodule, as in repository
;; scripts. Both assertions must run and be counted.
(define (main-only-case dir)
  (define path
    (write-fixture! dir
                    "main-only.rkt"
                    #<<FIXTURE
#lang racket
(require rackunit)
(module+ main
  (check-true #t)
  (check-equal? (+ 1 1) 2))
FIXTURE
                    ))
  (define result (car (run-quick (list path) 1)))
  (check-true (quick-result-valid? result) (quick-result-stderr result))
  (check-equal? (quick-result-failed result) 0)
  (check-equal? (quick-result-total result) 2)
  (check-false (quick-result-reason result)))

;; (2) test + main: exactly one entry point runs. Repository tests commonly
;; declare the same suite in both submodules, so running both would execute and
;; count every assertion twice. The fixture mirrors that shape with a failing
;; main assertion plus a marker log: the run stays valid, counts one assertion,
;; and logs only "test".
(define (test-and-main-case dir)
  (define marks (build-path dir "marks.txt"))
  (define path
    (write-fixture! dir
                    "test-and-main.rkt"
                    (format #<<FIXTURE
#lang racket
(require rackunit)
(define (mark tag)
  (call-with-output-file ~s #:exists 'append (lambda (out) (displayln tag out))))
(module+ test
  (mark "test")
  (check-true #t))
(module+ main
  (mark "main")
  (check-true #f))
FIXTURE
                            (path->string marks))))
  (define result (car (run-quick (list path) 1)))
  (check-true (quick-result-valid? result) (quick-result-stderr result))
  (check-equal? (quick-result-failed result) 0)
  (check-equal? (quick-result-total result) 1)
  (check-equal? (file->lines marks) '("test")))

;; (2b) main-only keeps working when test is absent; the main entry point is
;; the repository's script convention.
(define (main-preferred-when-no-test-case dir)
  (define path
    (write-fixture!
     dir
     "main-entry.rkt"
     "#lang racket\n(require rackunit)\n(module+ main (check-true #t) (check-true #t))\n"))
  (define result (car (run-quick (list path) 1)))
  (check-true (quick-result-valid? result) (quick-result-stderr result))
  (check-equal? (quick-result-total result) 2))
;; (3) neither submodule: the module body is the entry point.
(define (body-only-case dir)
  (define path
    (write-fixture! dir "body-only.rkt" "#lang racket\n(require rackunit)\n(check-true #t)\n"))
  (define result (car (run-quick (list path) 1)))
  (check-true (quick-result-valid? result) (quick-result-stderr result))
  (check-equal? (quick-result-total result) 1))

;; (4a) A slow file gets more than the run default from its own entry.
(define (per-file-timeout-allows-slow-file dir)
  (define path
    (write-fixture! dir "slow.rkt" "#lang racket\n(require rackunit)\n(sleep 3)\n(check-true #t)\n"))
  (define result (car (run-quick (list path) 1 #:timeout 1 #:timeouts (hash path 60))))
  (check-true (quick-result-valid? result) (quick-result-stderr result))
  (check-equal? (quick-result-total result) 1))

;; (4b) A short per-file timeout wins over a generous run default.
(define (short-per-file-timeout-case dir)
  (define path
    (write-fixture! dir
                    "never.rkt"
                    "#lang racket\n(require rackunit)\n(check-true #t)\n(sync never-evt)\n"))
  (define result (car (run-quick (list path) 1 #:timeout 120 #:timeouts (hash path 2))))
  (check-false (quick-result-valid? result))
  (check-eq? (quick-result-reason result) 'timeout)
  (check-not-equal? (quick-result-exit result) 0)
  (check-regexp-match #rx"Command timed out" (quick-result-stderr result)))

;; (4c) Bad timeout tables are rejected before any child starts.
(define (timeouts-validation-case dir)
  (define path (write-fixture! dir "valid.rkt" "#lang racket\n(require rackunit)\n(check-true #t)\n"))
  (check-exn #rx"#:timeouts" (lambda () (run-quick (list path) 1 #:timeouts '("x" . 1))))
  (check-exn #rx"#:timeouts" (lambda () (run-quick (list path) 1 #:timeouts (hash path 0))))
  (check-exn #rx"#:timeouts"
             (lambda () (run-quick (list path) 1 #:timeouts (hash (string->symbol path) 5)))))

;; (5) The child wrote valid evidence but reported a non-zero exit: the result
;; keeps the real exit code and is invalid with reason 'exit.
(define (child-exit-case dir)
  (define path
    (write-fixture! dir "valid-then-exit.rkt" "#lang racket\n(require rackunit)\n(check-true #t)\n"))
  (define result
    (parameterize ([current-quick-result-transform
                    (lambda (raw) (struct-copy subprocess-result raw [exit-code 3]))])
      (car (run-quick (list path) 1))))
  (check-equal? (quick-result-failed result) 0)
  (check-equal? (quick-result-total result) 1)
  (check-false (quick-result-valid? result))
  (check-equal? (quick-result-exit result) 3)
  (check-eq? (quick-result-reason result) 'exit))

;; (6) Relative spellings normalize to the same identity; duplicates still fail.
(define (normalization-case dir)
  (define path
    (write-fixture! dir "normalize.rkt" "#lang racket\n(require rackunit)\n(check-true #t)\n"))
  (define expected (path->string (simplify-path (path->complete-path path))))
  (parameterize ([current-directory dir])
    (define result (car (run-quick (list "./normalize.rkt") 1)))
    (check-true (quick-result-valid? result) (quick-result-stderr result))
    (check-equal? (quick-result-path result) expected)
    (check-exn #rx"duplicate inputs" (lambda () (run-quick (list path "./normalize.rkt") 1)))))

(module+ test
  (if (quick-supported?)
      (let ([dir (make-temporary-file "qt-child-modes-~a" 'directory)])
        (dynamic-wind
         void
         (lambda ()
           (test-case "main-only file runs and reports its counts"
             (main-only-case dir))
           (test-case "test and main submodules never run an assertion twice"
             (test-and-main-case dir))
           (test-case "main-only file runs through the main entry point"
             (main-preferred-when-no-test-case dir))
           (test-case "file with neither submodule still runs its body"
             (body-only-case dir))
           (test-case "per-file timeout allows a file to outrun the default"
             (per-file-timeout-allows-slow-file dir))
           (test-case "short per-file timeout reports reason 'timeout"
             (short-per-file-timeout-case dir))
           (test-case "non-zero child exit after valid evidence is invalid with reason 'exit"
             (child-exit-case dir))
           (test-case "relative input normalizes and duplicates are rejected"
             (normalization-case dir))
           (test-case "#:timeouts is validated before any child starts"
             (timeouts-validation-case dir)))
         (lambda () (delete-directory/files dir))))
      (displayln "SKIP qt child modes: Linux /proc or setsid unavailable")))
