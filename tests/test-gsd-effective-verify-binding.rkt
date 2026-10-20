#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; New API contract tests, not historical/live behavioral RED evidence.
(require rackunit
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/review-proof-format.rkt"
         "../extensions/gsd/review-audit-format.rkt"
         "../util/json/checksum.rkt")

(define stages
  '("raco fmt -i runtime/settings-query.rkt extensions/gsd/wave-executor.rkt"
    "raco make runtime/settings-query.rkt extensions/gsd/wave-executor.rkt"
    "racket scripts/run-tests.rkt tests/test-gsd-wave-executor-isolation.rkt tests/test-gsd-wave-worktree.rkt tests/test-arch-parameters.rkt"
    "racket scripts/run-tests.rkt --suite fast"
    "racket scripts/lint-all.rkt"))
(define c0 (string-join stages " && "))
(define test-path "tests/test-new-local-witness.rkt")
(define c1 (make-effective-verify-command c0 test-path))
(define (change-stage command index value)
  (string-join (list-set (owned-chain-stages command) index value) " && "))

(module+ test
  (test-case "C1 adds exactly one focused argument, retaining all five gates"
    (check-true (effective-verify-superset? c0 c1 test-path))
    (check-equal? (owned-chain-stages c1)
                  (list-set stages 2 (string-append (list-ref stages 2) " " test-path)))
    (check-equal? (make-effective-verify-command (string-join stages "\n") test-path) c1)
    (check-true (effective-verify-superset? c0 (string-join (owned-chain-stages c1) "\n") test-path)))
  (test-case "trusted jobs 2 is retained, never injected by the amendment"
    (define jobs-base (change-stage c0 3 "racket scripts/run-tests.rkt --suite fast --jobs 2"))
    (check-true (effective-verify-superset? jobs-base
                                            (make-effective-verify-command jobs-base test-path)
                                            test-path))
    (check-false (effective-verify-superset?
                  c0
                  (change-stage c1 3 "racket scripts/run-tests.rkt --suite fast --jobs 2")
                  test-path)))
  (test-case "unsafe new operands refuse construction and validation"
    (for ([path (list #f
                      ""
                      "--suite"
                      "/tests/x.rkt"
                      "tests/../x.rkt"
                      "tests/./x.rkt"
                      "tests//x.rkt"
                      "tests/-x.rkt"
                      "tests/x.rkt --help"
                      "tests/x.rkt;true"
                      "tests/x.rkt&&true"
                      "tests/$(id).rkt"
                      "tests/`id`.rkt"
                      "tests/*.rkt"
                      "tests/x\\.rkt"
                      "tests/x.rkt\ntrue"
                      "tests/x.rkt|true"
                      "tests/x.rkt>out"
                      "runtime/x.rkt"
                      "tests/x.txt")])
      (check-exn exn:fail?
                 (lambda () (make-effective-verify-command c0 path))
                 (format "path ~s" path))
      (check-false (effective-verify-superset? c0 c1 path))))
  (test-case "duplicate new test is not supplemental"
    (check-exn exn:fail? (lambda () (make-effective-verify-command c1 test-path)))
    (check-false (effective-verify-superset? c1 c1 test-path)))
  (test-case "all gate removal, reordering, narrowing and shell changes refuse"
    (define focused (list-ref (owned-chain-stages c1) 2))
    (define bad-commands
      (list c0
            (string-append c1 " && true")
            (string-join (cdr (owned-chain-stages c1)) " && ")
            (string-join (reverse (owned-chain-stages c1)) " && ")
            (change-stage c1 0 "raco fmt -i runtime/other.rkt")
            (change-stage c1 1 "raco make runtime/other.rkt")
            (change-stage c1 2 (string-append "racket scripts/run-tests.rkt " test-path))
            (change-stage c1 2 (string-replace focused "tests/test-gsd-wave-worktree.rkt " ""))
            (change-stage c1 2 (string-append focused " tests/extra.rkt"))
            (change-stage c1 2 (string-append focused " --jobs 2"))
            (change-stage c1 2 (string-append focused " --suite fast"))
            (change-stage c1 2 (string-append focused " " test-path))
            (change-stage c1 3 "racket scripts/run-tests.rkt --suite slow")
            (change-stage c1 3 "racket scripts/run-tests.rkt --suite fast --jobs 3")
            (change-stage c1 4 "racket scripts/lint-format.rkt")
            (string-append "cd repo && " c1)
            (string-append c1 "; true")
            (string-replace c1 " && " " || ")))
    (for ([bad bad-commands])
      (check-false (effective-verify-superset? c0 bad test-path) bad)))
  (test-case "invalid C0 shapes refuse even if purported C1 matches"
    (for ([bad (list #f
                     ""
                     "true"
                     (string-append c0 " && true")
                     (string-join (reverse stages) " && ")
                     (change-stage c0 0 "raco fmt -i ../outside.rkt")
                     (change-stage c0 2 "racket scripts/run-tests.rkt tests/../outside.rkt")
                     (change-stage c0 4 "echo lint"))])
      (check-exn exn:fail? (lambda () (make-effective-verify-command bad test-path)))
      (check-false (effective-verify-superset? bad c1 test-path))))
  (test-case "actual audit extractor accepts synthetic C1 five-stage manifest"
    ;; Schema fixture only: these facts do not assert any command was executed.
    (define binding
      (hasheq 'repo
              "/fixture/repo"
              'branch
              "fixture-work"
              'origin
              "https://github.com/local/fixture"
              'logical-repository
              "local/fixture"
              'attempt-id
              "fixture-attempt"
              'attempt-fence
              2
              'head
              (make-string 40 #\a)
              'tree
              (make-string 40 #\b)
              'plan-id
              (make-string 64 #\c)
              'snapshot-sha256
              (make-string 64 #\d)
              'wave
              1))
    (define source
      (for/hasheq ([key '(repo branch origin head tree)])
        (values key (hash-ref binding key))))
    (define argv (list "/bin/sh" "-c" c1))
    (define log-sha (make-string 64 #\e))
    (define manifest
      (hasheq 'schema-version
              1
              'kind
              "declared-verify-green"
              'verification-origin
              "owned-registry"
              'binding
              binding
              'before
              source
              'after
              source
              'execution-cwd
              "/fixture/repo"
              'execution-base
              "/fixture"
              'execution-worktree
              #t
              'frozen-normalized-command
              c1
              'command
              c1
              'argv
              argv
              'command-sha256
              (sha256-string (string-join argv "\u0"))
              'log
              (hasheq 'sha256 log-sha 'size 1 'path (string-append log-sha ".log"))
              'job
              (hasheq 'id
                      "fixture-job"
                      'state
                      "completed"
                      'exit-code
                      0
                      'reaped
                      #t
                      'fresh
                      #t
                      'pid
                      1
                      'proc-start-ticks
                      1
                      'start-ms
                      1
                      'end-ms
                      2
                      'identity-key
                      (sha256-string
                       (string-join (list "/fixture" "W1" "delivery-verify" "/fixture/repo" c1)
                                    "\u0")))))
    (check-true (valid-owned-proof-manifest? manifest))
    (define gates (owned-green-gates manifest))
    (check-equal? (sort (hash-keys gates) symbol<?) '(fast focused-tests format-compile lint))
    (check-equal? (hash-ref (hash-ref gates 'focused-tests) 'command)
                  (list-ref (owned-chain-stages c1) 2))
    (check-equal? (hash-ref (hash-ref gates 'format-compile) 'command)
                  (string-join (take stages 2) " && "))
    (check-equal? (hash-ref (hash-ref gates 'fast) 'command) (list-ref stages 3))
    (check-equal? (hash-ref (hash-ref gates 'lint) 'command) (list-ref stages 4))))
