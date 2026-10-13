#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @isolation temp-dir
;; @covers extensions/gsd/review-audit-format.rkt extensions/gsd/review-proof.rkt
(require rackunit
         racket/runtime-path
         "../extensions/gsd/review-audit-format.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/plan-snapshot.rkt")
(module+ test
  (test-case "trusted jobs 2 expands real --suite fast and legacy positional form only"
    (define suite "cd <project-base>/q && racket scripts/run-tests.rkt --suite fast")
    (define legacy "cd <project-base>/q && racket scripts/run-tests.rkt fast")
    (check-equal? (apply-verify-jobs suite 2)
                  "cd <project-base>/q && racket scripts/run-tests.rkt --suite fast --jobs 2")
    (check-equal? (apply-verify-jobs legacy 2)
                  "cd <project-base>/q && racket scripts/run-tests.rkt fast --jobs 2")
    (check-equal? (apply-verify-jobs (apply-verify-jobs suite 2) 2) (apply-verify-jobs suite 2))
    (check-exn exn:fail? (lambda () (apply-verify-jobs suite 3)))
    (check-exn exn:fail? (lambda () (apply-verify-jobs (string-append suite " --jobs 3") 2)))
    (check-equal? (expand-frozen-command suite "/parent" "/checkout" #t #:jobs 2)
                  "cd '/checkout' && racket scripts/run-tests.rkt --suite fast --jobs 2"))
  (test-case "the real --suite flag is the full fast suite; every other selection is not"
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt --suite fast"))
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt --suite fast --jobs 2"))
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt --suite=fast"))
    (for ([flag '("--suite unit" "--suite impact"
                                 "--suite=canary"
                                 "--suite fast --files a.rkt"
                                 "--suite fast tests/a.rkt"
                                 "--suite fast --profile canary"
                                 "--suite fast --shard-plan report")])
      (check-false (proof-verify-command-eligible? (string-append "racket scripts/run-tests.rkt "
                                                                  flag))
                   flag))
    (check-false
     (proof-verify-command-eligible?
      "racket scripts/run-tests.rkt --suite fast && racket scripts/run-tests.rkt fast x.rkt"))
    ;; Partial selections are refused at the RETENTION layer regardless of
    ;; operand order: files before the suite flag, shard flags before it,
    ;; or focused files carrying any flag at all.
    (for ([command (list "racket scripts/run-tests.rkt tests/a.rkt --suite fast"
                         "racket scripts/run-tests.rkt --shard-index 0 --shard-total 2 --suite fast"
                         "racket scripts/run-tests.rkt --suite fast --jobs 2 tests/a.rkt"
                         "racket scripts/run-tests.rkt tests/a.rkt --jobs 2")])
      (check-false (proof-verify-command-eligible? command) command))
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt fast"))
    (check-true (proof-verify-command-eligible? "racket scripts/run-tests.rkt tests/a.rkt")))
  (test-case "owned chain stages and focused RED command grammar"
    (define chain
      (string-append "cd <project-base>/q && raco fmt -i a.rkt b.rkt && raco make a.rkt"
                     " && racket scripts/run-tests.rkt tests/a.rkt tests/b.rkt"
                     " && racket scripts/run-tests.rkt --suite fast"
                     " && racket scripts/lint-all.rkt"))
    (check-equal? (length (owned-chain-stages chain)) 6)
    (check-true (owned-red-focused-command? "racket scripts/run-tests.rkt tests/a.rkt tests/b.rkt"))
    (check-true (owned-red-focused-command? "raco test tests/a.rkt"))
    (check-true (owned-red-focused-command? "racket tests/a.rkt"))
    (check-false (owned-red-focused-command? "racket scripts/run-tests.rkt --suite fast"))
    (check-false (owned-red-focused-command? "racket scripts/run-tests.rkt tests/a.rkt --jobs 2"))
    (check-false (owned-red-focused-command? "racket scripts/run-tests.rkt src/a.rkt")))
  (test-case "exact audit binding and acknowledgement are not execution or approval"
    (define expected (audit-binding (hasheq 'head "source" 'tree "tree" 'generation 0)))
    (define ack
      (hasheq 'mode
              owned-audit-mode
              'verification-origin
              "audited-owned"
              'source-read
              #t
              'raw-output-read
              #t
              'binding
              expected))
    (check-true (valid-audit-ack? ack expected))
    (for ([key '(mode verification-origin source-read raw-output-read binding)])
      (check-false (valid-audit-ack? (hash-remove ack key) expected)))
    (check-false (valid-audit-ack? ack (hash-set expected 'generation 1))))
  (test-case "a meaningful RED acknowledgement names the linked red and its raw output"
    (check-true (valid-audit-red-ack? (hasheq 'proof-sha256 (make-string 64 #\a) 'raw-output-read #t)
                                      (make-string 64 #\a)))
    (check-false (valid-audit-red-ack? (hasheq 'proof-sha256 (make-string 64 #\b) 'raw-output-read #t)
                                       (make-string 64 #\a)))
    (check-false (valid-audit-red-ack? (hasheq 'proof-sha256 (make-string 64 #\a) 'raw-output-read #f)
                                       (make-string 64 #\a))))
  (test-case "BUG-0081 frozen canonical arrow selects custom carrier not W-number prefix"
    (define root (make-temporary-file "audit-arrow-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define plan (make-string 64 #\a))
                    (make-directory* (build-path root ".planning" "waves"))
                    (define text
                      "# Plan: custom\n\n## Waves\n\n- [Inbox] W1: Own wave → waves/W9-carrier.md\n")
                    (display-to-file text (build-path root ".planning" "PLAN.md"))
                    (display-to-file "# Wave 9\n\n## Verify\nprintf 'correct frozen carrier'\n"
                                     (build-path root ".planning" "waves" "W9-carrier.md"))
                    (display-to-file "# Wave 1\n\n## Verify\nprintf 'wrong filename guess'\n"
                                     (build-path root ".planning" "waves" "W1-decoy.md"))
                    (make-plan-snapshot! root plan text)
                    (check-equal? (hash-ref (frozen-verify-binding root plan 1) 'normalized-command)
                                  "printf 'correct frozen carrier'"))
                  (lambda () (delete-directory/files root)))))
