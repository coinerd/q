#lang racket/base
;; Pure audit schema. Neither model JSON nor these checks authenticate execution.
(require racket/list
         racket/string
         "review-proof-format.rkt")
(provide owned-audit-mode
         audit-binding
         valid-audit-ack?
         valid-audit-red-ack?
         owned-green-gates)
(define owned-audit-mode "owned-proof-audit-v1")
(define audit-keys
  '(plan-id wave generation head tree branch attempt-id attempt-fence content-digest proof-sha256))
(define (audit-binding request)
  (for/hasheq ([key (in-list audit-keys)])
    (values key (hash-ref request key #f))))
(define (valid-audit-ack? ack expected)
  (and (hash? ack)
       (equal? (hash-ref ack 'mode #f) owned-audit-mode)
       (equal? (hash-ref ack 'verification-origin #f) "audited-owned")
       (eq? (hash-ref ack 'source-read #f) #t)
       (eq? (hash-ref ack 'raw-output-read #f) #t)
       (equal? (hash-ref ack 'binding #f) expected)))
;; The peer must meaningfully acknowledge the linked behavioral RED — the
;; exact retained red manifest identity plus its actual raw failure output —
;; never a boolean receipt of the green approval.
(define (valid-audit-red-ack? red-ack red-sha)
  (and (hash? red-ack)
       (equal? (hash-ref red-ack 'proof-sha256 #f) red-sha)
       (eq? (hash-ref red-ack 'raw-output-read #f) #t)))
;; Conservative approved chain grammar. Every stage must be a command shape
;; the repository actually supports: the frozen W1 Verify chain is
;;   raco fmt -i <files> / raco make <files> /
;;   racket scripts/run-tests.rkt <focused test files> /
;;   racket scripts/run-tests.rkt --suite fast / racket scripts/lint-all.rkt
;; (plus the real lint-format/metrics spellings used by earlier frozen waves).
;; Unsupported chains stop; exit zero alone must never be used to guess
;; per-stage coverage from logs, and no fictional command (e.g. a
;; nonexistent `raco fmt-strict`) is accepted as an executed gate.
(define (owned-green-gates manifest)
  (define normalized (hash-ref manifest 'frozen-normalized-command ""))
  (define stages (owned-chain-stages normalized))
  (define (refuse)
    (error 'owned-audit "unsupported-owned-gate-chain"))
  (unless (and (valid-owned-proof-manifest? manifest)
               stages
               (not (regexp-match? #px"[;|\n`$()<>]"
                                   (string-replace normalized "<project-base>" "BASE")))
               (andmap (lambda (s) (regexp-match? #px"^(?:cd |racket |raco )[^&]+$" s)) stages))
    (refuse))
  (define (find rx)
    (define matches (filter (lambda (s) (regexp-match? rx s)) stages))
    (unless (= 1 (length matches))
      (refuse))
    (car matches))
  ;; Focused stage: whole positional test files, no flags (frozen W1 shape).
  (define focused
    (find #px"^racket scripts/run-tests[.]rkt tests/[^ ]+[.]rkt(?: tests/[^ ]+[.]rkt)*$"))
  ;; Format: the REAL in-place canonical formatter (`raco fmt -i`), not a
  ;; fictional strict variant. Compile: raco make over real files.
  (define fmt (find #px"^raco fmt -i [^ ]+[.]rkt(?: [^ ]+[.]rkt)*$"))
  (define compile (find #px"^raco make [^ ]+[.]rkt(?: [^ ]+[.]rkt)*$"))
  ;; Lint: the repository's real whole-tree lint entrypoints.
  (define lint (find #px"^racket scripts/(?:lint-all|lint-format|metrics)[.]rkt$"))
  ;; Broad: the full fast suite through the real CLI suite flag, with only
  ;; the trusted operational --jobs N parameter.
  (define fast (find #px"^racket scripts/run-tests[.]rkt --suite fast(?: --jobs [1-9][0-9]*)?$"))
  (unless (and (proof-verify-command-eligible? normalized)
               (owned-red-focused-command? focused)
               (= (length stages)
                  (+ 5 (if (and (pair? stages) (string-prefix? (car stages) "cd ")) 1 0))))
    (refuse))
  (define (fact command)
    (hasheq 'result
            "passed"
            'command
            command
            'verification-origin
            "audited-owned"
            'compound-command
            (hash-ref manifest 'command)
            'owner-job-id
            (hash-ref (hash-ref manifest 'job) 'id)))
  (hasheq 'focused-tests
          (fact focused)
          'format-compile
          (fact (string-append fmt " && " compile))
          'lint
          (fact lint)
          'fast
          (fact fast)))
