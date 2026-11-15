#lang racket/base
;; @suite testing
;; @speed fast
;; @boundary unit
;; @isolation process

;; Contract: the runner NEVER rewrites managed surfaces (info.rkt, README.md,
;; CHANGELOG.md). It snapshots them, and if a test changes one it fails the
;; suite and leaves the bytes alone — restoring them would destroy
;; pre-existing or concurrent user edits. Mutation coverage belongs in
;; temporary fixtures.

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/system)

(define-runtime-path runner "../scripts/run-tests/runner.rkt")

(define (put p b)
  (call-with-output-file p #:exists 'truncate/replace (lambda (out) (write-bytes b out))))

(define (fixture proc)
  (define dir (make-temporary-file "surface-fixture-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory (build-path dir "scripts"))
                  (make-directory (build-path dir "tests"))
                  (put (build-path dir "scripts" "run-tests.rkt") #"")
                  (proc dir))
                (lambda () (delete-directory/files dir))))

;; kind: 'clean (no surface writes), 'mutate (writes README + CHANGELOG), or
;; 'exception (writes, then the worker is killed mid-flight).
(define (surface-writes-for dir kind)
  (if (eq? kind 'clean)
      ""
      (string-append
       (format "(call-with-output-file ~s #:exists 'truncate (lambda (p) (display \"mutated\" p)))\n"
               (path->string (build-path dir "README.md")))
       (format "(call-with-output-file ~s #:exists 'truncate void)\n"
               (path->string (build-path dir "CHANGELOG.md"))))))

(define (write-mutator! dir kind)
  (put (build-path dir "tests" "test-mutator.rkt")
       (string->bytes/utf-8
        (string-append "#lang racket/base\n;; @isolation process\n"
                       (surface-writes-for dir kind)
                       (format "(call-with-output-file ~s #:exists 'truncate void)\n"
                               (path->string (build-path dir "ready")))
                       (if (eq? kind 'exception) "(sleep 30)\n" "")))))

(define (driver-source dir kind)
  (define head
    (format (string-append
             "#lang racket/base\n(require racket/list racket/async-channel)\n"
             "(define run (dynamic-require '(file ~s) 'run-suite-once))\n"
             "(define (go) (run (list ~s) 1 30000 #f \"explicit\" 1 1 'subprocess #f #f 'vps))\n")
            (path->string runner)
            (path->string (build-path dir "tests" "test-mutator.rkt"))))
  (if (eq? kind 'exception)
      (string-append
       head
       "(define cust (make-custodian))\n"
       "(define result (make-async-channel))\n"
       "(define (threads c p) (append-map (lambda (v) (cond [(thread? v) (list v)] [(custodian? v) (threads v c)] "
       "[else '()])) (custodian-managed-list c p)))\n"
       "(define (worker) (findf (lambda (t) (assq 'run-single-file/subprocess "
       "(continuation-mark-set->context (continuation-marks t)))) "
       "(threads cust (current-custodian))))\n"
       "(define coordinator (parameterize ([current-custodian cust])\n"
       "  (thread (lambda () (async-channel-put result (with-handlers ([exn? values]) (call-with-values go list)))))))\n"
       "(define deadline (+ (current-inexact-milliseconds) 20000))\n"
       "(define (spin) (unless (and (file-exists? \"ready\") (worker)) "
       "(when (> (current-inexact-milliseconds) deadline) (error 'fixture \"timeout\")) "
       "(sleep 0.01) (spin)))\n"
       "(spin)\n"
       "(kill-thread (worker))\n"
       "(sync/timeout 10 result)\n"
       "(call-with-output-file \"scheduler-ran\" #:exists 'truncate void)\n"
       "(exit 3)\n")
      (string-append head "(go)\n")))

(define (run-fixture dir kind)
  (write-mutator! dir kind)
  (define driver (build-path dir "driver.rkt"))
  (put driver (string->bytes/utf-8 (driver-source dir kind)))
  (parameterize ([current-directory dir]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (system*/exit-code (find-executable-path "racket") driver)))

(module+ test
  (test-case "clean run succeeds and leaves managed surfaces byte-identical"
    (fixture (lambda (dir)
               (define info (build-path dir "info.rkt"))
               (define readme (build-path dir "README.md"))
               (define changelog (build-path dir "CHANGELOG.md"))
               (put info #"user\0bytes\377\n")
               (put readme #"uncommitted readme\n")
               (file-or-directory-permissions info #o640)
               (check-equal? (run-fixture dir 'clean) 0)
               (check-true (file-exists? (build-path dir "ready")))
               (check-equal? (file->bytes info) #"user\0bytes\377\n")
               (check-equal? (file->bytes readme) #"uncommitted readme\n")
               (check-equal? (file-or-directory-permissions info 'bits) #o640)
               (check-false (file-exists? changelog)))))

  (test-case "mutating run fails closed and never rewrites user bytes"
    (fixture (lambda (dir)
               (define info (build-path dir "info.rkt"))
               (define readme (build-path dir "README.md"))
               (define changelog (build-path dir "CHANGELOG.md"))
               (put info #"user\0bytes\377\n")
               (put readme #"uncommitted readme\n")
               (file-or-directory-permissions info #o640)
               (check-not-equal? (run-fixture dir 'mutate) 0)
               (check-true (file-exists? (build-path dir "ready")))
               ;; The test's own writes stay visible: the runner never reverts them.
               (check-equal? (file->bytes readme) #"mutated")
               (check-true (file-exists? changelog))
               (check-equal? (file->bytes info) #"user\0bytes\377\n")
               (check-equal? (file-or-directory-permissions info 'bits) #o640))))

  (test-case "scheduler exception still cannot destroy user bytes"
    (fixture (lambda (dir)
               (define info (build-path dir "info.rkt"))
               (define readme (build-path dir "README.md"))
               (put info #"user\0bytes\377\n")
               (put readme #"uncommitted readme\n")
               (check-not-equal? (run-fixture dir 'exception) 0)
               (check-true (file-exists? (build-path dir "scheduler-ran")))
               (check-equal? (file->bytes info) #"user\0bytes\377\n"))))

  (test-case "symlink surface fails closed before any test runs"
    (fixture (lambda (dir)
               (define target (build-path dir "target"))
               (put target #"untouched")
               (make-file-or-directory-link target (build-path dir "info.rkt"))
               (check-not-equal? (run-fixture dir 'clean) 0)
               (check-equal? (file->bytes target) #"untouched")
               (check-true (link-exists? (build-path dir "info.rkt")))
               (check-false (file-exists? (build-path dir "ready"))))))

  (test-case "directory surface fails closed"
    (fixture (lambda (dir)
               (make-directory (build-path dir "README.md"))
               (check-not-equal? (run-fixture dir 'clean) 0)
               (check-true (directory-exists? (build-path dir "README.md")))
               (check-false (file-exists? (build-path dir "ready")))))))
