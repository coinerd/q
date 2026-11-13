#lang racket/base
;; @covers extensions/gsd/verify-command-format.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;; @timeout 120
;; Pure normalization decisions; fixture-backed and timeout decisions
;; live in test-gsd-delivery-verifier-decision-integration.rkt.

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         (only-in "../extensions/gsd/verify-command-format.rkt"
                  normalize-declared-verify
                  verify-declaration-extraction))

(define-runtime-path composition-root "../extensions/gsd/composition-root.rkt")
(define-runtime-path verification-job "../extensions/gsd/verification-job.rkt")
(define-runtime-path subprocess "../sandbox/subprocess.rkt")

(define (decision-suite)
  (test-suite "delivery-verifier pure decisions"
    (test-case "pure owner does not load bootstrap or process modules"
      (for ([path (in-list (list composition-root verification-job subprocess))])
        (check-false (module-declared? `(file ,(path->string path))) (path->string path))))
    ;; BUG-0068 secondary finding 2: markdown-decorated Verify criteria
    ;; (prose bullets + inline-code spans) must normalize into runnable
    ;; commands; canonical declarations pass through byte-for-byte.
    (test-case "normalize-declared-verify extracts code spans from prose bullets"
      (check-equal?
       (normalize-declared-verify
        "- `racket tests/test-shell-risk.rkt`, `racket tests/test-tool-bash-security.rkt` green.")
       "racket tests/test-shell-risk.rkt && racket tests/test-tool-bash-security.rkt"))
    (test-case "normalize-declared-verify joins one-command-per-bullet with &&"
      (check-equal? (normalize-declared-verify "- `racket a.rkt`\n- `racket b.rkt`")
                    "racket a.rkt && racket b.rkt"))
    (test-case "normalize-declared-verify passes canonical commands through unchanged"
      (define canonical "cd <project-base>/q && racket scripts/metrics.rkt --lint")
      (check-equal? (normalize-declared-verify canonical) canonical))
    (test-case "normalize-declared-verify passes plain commands through unchanged"
      (check-equal? (normalize-declared-verify "racket tests/test-foo.rkt")
                    "racket tests/test-foo.rkt"))
    (test-case "BUG-0070: ignores prose identifiers and extracts late executable spans"
      (check-equal?
       (normalize-declared-verify
        (string-append
         "- unknown stays `unknown`; classification is `distinct_*`.\n"
         "- criteria two; criteria three; criteria four; criteria five; criteria six.\n"
         "- `racket scripts/run-tests.rkt --suite fast` green; `metrics.rkt --lint` green."))
       (string-append "racket scripts/run-tests.rkt --suite fast"
                      " && racket scripts/metrics.rkt --lint")))
    (test-case "BUG-0070: prose-only bullets do not become shell"
      (check-equal? (normalize-declared-verify "- unknown stays `unknown`.") ""))

    ;; R75 (BUG-0075): the parser now preserves section newlines, so a
    ;; prose-first bullet section reaches the normalizer DECORATED and only
    ;; its authored spans normalize — the historical "; " join hid every
    ;; bullet anchor after the first line and passed the whole section to
    ;; /bin/sh verbatim.
    (test-case "R75: prose-before-bullets extracts spans instead of becoming shell"
      (check-equal?
       (normalize-declared-verify (string-append "The gates below must all pass before delivery.\n"
                                                 "- `racket tests/test-shell-risk.rkt` green.\n"
                                                 "- `metrics.rkt --lint` green."))
       "racket tests/test-shell-risk.rkt && racket scripts/metrics.rkt --lint"))
    (test-case "R75: unsupported command-shaped spans fail visibly among valid ones"
      (define declaration "- `racket a.rkt` green\n- `grep -L forbidden q/report.txt` clean\n")
      (check-equal? (normalize-declared-verify declaration) "")
      (check-equal? (hash-ref (car (hash-ref (verify-declaration-extraction declaration) 'rejections))
                              'reason)
                    'grep-exit-semantics))
    (test-case "R75: test and POSIX bracket spans are executable vocabulary"
      (check-equal? (normalize-declared-verify "- `test -f a.rkt`\n- `[ -f b.rkt ]` exists")
                    "test -f a.rkt && [ -f b.rkt ]"))))

(unless (= 0 (run-tests (decision-suite)))
  (error (quote decision-suite) "tests failed"))
