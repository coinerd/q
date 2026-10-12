#lang racket/base
;; Portable structural laws. No filesystem/process/model authority here.
(require racket/list
         racket/string
         (only-in "../../util/json/checksum.rkt" sha256-string))
(provide proof-hex?
         valid-proof-file-reference?
         valid-owned-proof-reference?
         valid-owned-proof-manifest?
         proof-reference-matches-receipt?
         expand-frozen-command
         apply-verify-jobs
         proof-verify-command-eligible?
         proof-logical-repository
         owned-chain-stages
         owned-red-focused-command?
         valid-owned-red-reference?
         valid-owned-red-manifest?
         red-links-green-proof?)
(define (proof-hex? v n)
  (and (string? v) (= (string-length v) n) (regexp-match? #px"^[0-9a-f]+$" v)))
(define (text? v)
  (and (string? v) (positive? (string-length v))))
;; Trusted named operational option, not a model/explicit command override.
;; Only --jobs 2 is supported, and never a partial profile or file selection.
;; Both REAL runner spellings of the full suite are supported: the historical
;; positional `run-tests.rkt fast` form and the actual frozen CLI form
;; `run-tests.rkt --suite fast` (see scripts/run-tests/cli.rkt).
(define (apply-verify-jobs command jobs)
  (unless (member jobs '(#f 2))
    (error 'review-proof "unapproved operational jobs"))
  (cond
    [(not jobs) command]
    [else
     (when (regexp-match? #px"--jobs(?:=|[[:space:]])" command)
       (unless (regexp-match? #px"--jobs 2(?:[[:space:];&]|$)" command)
         (error 'review-proof "conflicting frozen jobs option")))
     (if (regexp-match? #px"--jobs 2(?:[[:space:];&]|$)" command)
         command
         (regexp-replace* #px"(scripts/run-tests[.]rkt (?:--suite fast|fast))(?=[[:space:];&]|$)"
                          command
                          "\\1 --jobs 2"))]))
(define (expand-frozen-command normalized base cwd worktree? #:jobs [jobs #f])
  (define (quote s)
    (string-append "'" (string-replace s "'" "'\"'\"'") "'"))
  (define q-root
    (if worktree?
        cwd
        (string-append (string-trim base "/" #:left? #f) "/q")))
  (apply-verify-jobs (string-replace (string-replace normalized "<project-base>/q" (quote q-root))
                                     "<project-base>"
                                     (quote base))
                     jobs))
(define (valid-proof-file-reference? r suffix)
  (and (hash? r)
       (proof-hex? (hash-ref r 'sha256 #f) 64)
       (exact-nonnegative-integer? (hash-ref r 'size #f))
       (equal? (hash-ref r 'path #f) (string-append (hash-ref r 'sha256) suffix))))
;; Scope/substitution flags that can narrow or replace the declared suite.
;; --suite is handled separately: the real CLI suite flag with value `fast`
;; IS the full-suite declaration; any other value is a selection.
(define verify-scope-flag-rx
  (pregexp
   (string-append
    "--(?:impact-dry-run|changed-base|changed|impact|partial|files?|help|inventory|diagnose-overhead|"
    "lint-metadata|generate-covers-manifest|explain|profile|shard-plan)(?:=|[[:space:]]|$)")))
;; Every --suite occurrence must select exactly the full fast suite, with no
;; extra operands beyond the trusted operational --jobs N.
(define (suite-selection-whole-fast? command)
  (for/and ([match (in-list (regexp-match* #px"--suite(?:=|[[:space:]]+)[^;&|]*" command))])
    (and
     (regexp-match? #px"^--suite(?:=|[[:space:]]+)fast(?:[[:space:]]|$)" match)
     (regexp-match?
      #px"^--suite(?:=|[[:space:]]+)fast(?:[[:space:]]+--jobs[[:space:]]+[1-9][0-9]*)?[[:space:]]*$"
      match))))
;; A declared chain can be retained without certifying a partial/dry-run fast
;; profile as broad green. Jobs is operational, not a profile substitution.
(define (proof-verify-command-eligible? command)
  (and
   (text? command)
   (not (regexp-match? verify-scope-flag-rx command))
   (suite-selection-whole-fast? command)
   (not
    (and
     (regexp-match? #px"(?:test|run-tests)[.]rkt[[:space:]]+fast(?:[[:space:]]|$)" command)
     (regexp-match?
      #px"(?:test|run-tests)[.]rkt[[:space:]]+fast[^;&|\n]*[[:space:]][^[:space:];&|]+[.]rkt(?:[[:space:];&|]|$)"
      command)))))
(define (proof-logical-repository origin)
  (and
   (string? origin)
   (regexp-match? #px"^(?:https://github\\.com/|git@github\\.com:)[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$"
                  origin)
   (let* ([slug (regexp-replace #px"^(?:https://github\\.com/|git@github\\.com:)" origin "")]
          [bare (regexp-replace #rx"[.]git$" slug "")])
     (string-downcase bare))))
(define (binding? b)
  (and (hash? b)
       (proof-logical-repository (hash-ref b 'origin #f))
       (equal? (hash-ref b 'logical-repository #f) (proof-logical-repository (hash-ref b 'origin #f)))
       (andmap (lambda (k) (text? (hash-ref b k #f))) '(repo branch origin attempt-id))
       (not (member (hash-ref b 'branch) '("main" "master")))
       (andmap (lambda (k) (proof-hex? (hash-ref b k #f) 40)) '(head tree))
       (andmap (lambda (k) (proof-hex? (hash-ref b k #f) 64)) '(plan-id snapshot-sha256))
       (andmap (lambda (k) (exact-nonnegative-integer? (hash-ref b k #f))) '(wave attempt-fence))))
;; A declared frozen chain is joined with fail-fast && stages in document
;; order; a zero exit therefore proves every preceding stage ran and passed,
;; never that only the last stage happened to succeed.
(define (owned-chain-stages normalized)
  (and (text? normalized) (map string-trim (string-split normalized " && "))))
;; A focused behavioral runner selects whole test files positionally and
;; takes no flags: `raco test tests/a.rkt ...`, `racket tests/a.rkt ...` or
;; the real runner facade `racket scripts/run-tests.rkt tests/a.rkt ...`
;; (the frozen W1 focused stage shape).
(define owned-red-focused-rx
  #px"^(?:raco test|racket|racket scripts/run-tests[.]rkt) tests/[^ ;&|]+[.]rkt(?: tests/[^ ;&|]+[.]rkt)*$")
(define (owned-red-focused-command? command)
  (and (text? command) (regexp-match? owned-red-focused-rx command)))
(define (valid-owned-proof-reference? r)
  (and (valid-proof-file-reference? r ".json")
       (equal? (hash-ref r 'schema-version #f) 1)
       (equal? (hash-ref r 'kind #f) "declared-verify-green")
       (binding? (hash-ref r 'binding #f))))
(define (proof-reference-matches-receipt? ref receipt)
  (and (valid-owned-proof-reference? ref)
       (hash? receipt)
       (for/and ([k '(repo branch origin head tree attempt-id attempt-fence)])
         (equal? (hash-ref (hash-ref ref 'binding) k #f) (hash-ref receipt k #f)))))
(define (valid-owned-proof-manifest? m)
  (and
   (hash? m)
   (equal? (hash-ref m 'schema-version #f) 1)
   (equal? (hash-ref m 'kind #f) "declared-verify-green")
   (equal? (hash-ref m 'verification-origin #f) "owned-registry")
   (binding? (hash-ref m 'binding #f))
   (equal? (hash-ref m 'before #f) (hash-ref m 'after #f))
   (hash? (hash-ref m 'before #f))
   (for/and ([k '(repo branch origin head tree)])
     (equal? (hash-ref (hash-ref m 'before) k #f) (hash-ref (hash-ref m 'binding) k #f)))
   (text? (hash-ref m 'execution-cwd #f))
   (text? (hash-ref m 'execution-base #f))
   (proof-verify-command-eligible? (hash-ref m 'command #f))
   (text? (hash-ref m 'frozen-normalized-command #f))
   (boolean? (hash-ref m 'execution-worktree 'missing))
   (with-handlers ([exn:fail? (lambda (_) #f)])
     (equal? (hash-ref m 'command)
             (expand-frozen-command (hash-ref m 'frozen-normalized-command)
                                    (hash-ref m 'execution-base)
                                    (hash-ref m 'execution-cwd)
                                    (hash-ref m 'execution-worktree)
                                    #:jobs (hash-ref m 'operational-jobs #f))))
   (equal? (hash-ref m 'argv #f) (list "/bin/sh" "-c" (hash-ref m 'command)))
   (equal? (hash-ref m 'command-sha256 #f) (sha256-string (string-join (hash-ref m 'argv) "\u0")))
   (valid-proof-file-reference? (hash-ref m 'log #f) ".log")
   (let ([j (hash-ref m 'job #f)])
     (and (hash? j)
          (text? (hash-ref j 'id #f))
          (equal? (hash-ref j 'identity-key #f)
                  (sha256-string (string-join (list (hash-ref m 'execution-base)
                                                    (format "W~a"
                                                            (hash-ref (hash-ref m 'binding) 'wave))
                                                    "delivery-verify"
                                                    (hash-ref m 'execution-cwd)
                                                    (hash-ref m 'command))
                                              "\u0")))
          (equal? (hash-ref j 'state #f) "completed")
          (equal? (hash-ref j 'exit-code #f) 0)
          (eq? (hash-ref j 'reaped #f) #t)
          (eq? (hash-ref j 'fresh #f) #t)
          (andmap (lambda (k) (exact-positive-integer? (hash-ref j k #f))) '(pid proc-start-ticks))
          (andmap (lambda (k) (and (real? (hash-ref j k #f)) (< 0 (hash-ref j k) +inf.0)))
                  '(start-ms end-ms))
          (<= (hash-ref j 'start-ms) (hash-ref j 'end-ms))))))
;; ---------------------------------------------------------------------------
;; Owned behavioral RED. A separate, weaker-but-real negative proof: a
;; fresh registered focused test runner execution that terminated in an
;; actual assertion failure. It can never stand in for the green manifest
;; (different kind, no constructor from prose/model JSON) and never carries
;; refresh or audit authority by itself.
;; ---------------------------------------------------------------------------
(define owned-red-kinds '("declared-red-failure"))
(define (valid-owned-red-reference? r)
  (and (valid-proof-file-reference? r ".json")
       (equal? (hash-ref r 'schema-version #f) 1)
       (equal? (hash-ref r 'kind #f) "declared-red-failure")
       (binding? (hash-ref r 'binding #f))))
(define (red-job-facts? j m)
  (and (hash? j)
       (text? (hash-ref j 'id #f))
       (equal? (hash-ref j 'identity-key #f)
               (sha256-string (string-join (list (hash-ref m 'execution-base)
                                                 (format "W~a" (hash-ref (hash-ref m 'binding) 'wave))
                                                 "delivery-red"
                                                 (hash-ref m 'execution-cwd)
                                                 (hash-ref m 'command))
                                           "\u0")))
       (equal? (hash-ref j 'state #f) "failed")
       ;; A real assertion failure, never exit 124 (timeout attribution) and
       ;; never a zero/garbage code: the registry records the child's own exit.
       (let ([code (hash-ref j 'exit-code #f)])
         (and (exact-integer? code) (> code 0) (not (= code 124))))
       (eq? (hash-ref j 'reaped #f) #t)
       (eq? (hash-ref j 'fresh #f) #t)
       (andmap (lambda (k) (exact-positive-integer? (hash-ref j k #f))) '(pid proc-start-ticks))
       (andmap (lambda (k) (and (real? (hash-ref j k #f)) (< 0 (hash-ref j k) +inf.0)))
               '(start-ms end-ms))
       (<= (hash-ref j 'start-ms) (hash-ref j 'end-ms))))
(define (valid-owned-red-manifest? m)
  (and (hash? m)
       (equal? (hash-ref m 'schema-version #f) 1)
       (equal? (hash-ref m 'kind #f) "declared-red-failure")
       (equal? (hash-ref m 'verification-origin #f) "owned-registry")
       (binding? (hash-ref m 'binding #f))
       (equal? (hash-ref m 'before #f) (hash-ref m 'after #f))
       (hash? (hash-ref m 'before #f))
       (for/and ([k '(repo branch origin head tree)])
         (equal? (hash-ref (hash-ref m 'before) k #f) (hash-ref (hash-ref m 'binding) k #f)))
       (text? (hash-ref m 'execution-cwd #f))
       (text? (hash-ref m 'execution-base #f))
       ;; The red command is the focused stage of the SAME frozen declaration the
       ;; green proof executes; it is not an arbitrary model-chosen command.
       (owned-red-focused-command? (hash-ref m 'focused-normalized-command #f))
       (member (hash-ref m 'focused-normalized-command #f)
               (owned-chain-stages (hash-ref m 'frozen-normalized-command #f)))
       (member (hash-ref m 'red-origin #f) '("preimplementation" "reproduction"))
       (boolean? (hash-ref m 'execution-worktree 'missing))
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (equal? (hash-ref m 'command)
                 (expand-frozen-command (hash-ref m 'focused-normalized-command)
                                        (hash-ref m 'execution-base)
                                        (hash-ref m 'execution-cwd)
                                        (hash-ref m 'execution-worktree)
                                        #:jobs #f)))
       (equal? (hash-ref m 'argv #f) (list "/bin/sh" "-c" (hash-ref m 'command)))
       (equal? (hash-ref m 'command-sha256 #f) (sha256-string (string-join (hash-ref m 'argv) "\u0")))
       (valid-proof-file-reference? (hash-ref m 'log #f) ".log")
       (red-job-facts? (hash-ref m 'job #f) m)
       #t))
;; The honest link between one green execution proof and one authentic RED
;; proof: same plan/wave/live attempt/fence/spec, distinct source identity
;; (the red tree cannot be the fixed tree), and a declared chronology the
;; recorded job timestamps must actually satisfy. A green capture cannot
;; mint or backdate this relation.
(define (red-links-green-proof? red green)
  (and
   (valid-owned-red-manifest? red)
   (valid-owned-proof-manifest? green)
   (let ([rb (hash-ref red 'binding)]
         [gb (hash-ref green 'binding)])
     (and (for/and ([k '(plan-id wave attempt-id attempt-fence snapshot-sha256 repo branch origin)])
            (equal? (hash-ref rb k #f) (hash-ref gb k #f)))
          (not (equal? (hash-ref rb 'head) (hash-ref gb 'head)))
          (not (equal? (hash-ref rb 'tree) (hash-ref gb 'tree)))))
   (let* ([rj (hash-ref red 'job)]
          [gj (hash-ref green 'job)])
     (if (equal? (hash-ref red 'red-origin) "preimplementation")
         (<= (hash-ref rj 'end-ms) (hash-ref gj 'start-ms))
         (>= (hash-ref rj 'start-ms) (hash-ref gj 'end-ms))))
   #t))
