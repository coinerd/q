#lang racket/base
;; @covers extensions/gsd/verify-command-grammar.rkt
;; @covers extensions/gsd/verify-command-format.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit
;; @timeout 120
;;
;; W1 Verify grammar consolidation — ONE low-level declaration grammar.
;;
;; These are focused characterization claims for the extracted grammar kernel
;; and for the delegation contract of `verify-command-format.rkt`:
;;   * the fail-closed chain splitter is unchanged (legacy single-line
;;     passthrough preserved; quoted delimiters are never boundaries;
;;     metacharacters, keywords, continuations and unbalanced quotes refuse),
;;   * the accepted/rejected span vocabulary and its canonical rewrites are
;;     unchanged,
;;   * the extraction result SHAPE is frozen and its rejections/refusals stay
;;     complete and named,
;;   * the format module re-exports the ONE grammar binding (no second
;;     parser),
;;   * the operator-reason taxonomy is closed, structured, additive, and
;;     never changes the extraction result.
;; End-to-end declaration/execution behavior lives in
;; tests/test-gsd-verify-declaration-contract.rkt and is not duplicated here.

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/verify-command-grammar.rkt"
                  verify-command-stages
                  canonical-verify-command
                  verify-first-token
                  verify-classify-span
                  verify-decorated?
                  verify-accept-first-tokens
                  verify-normalizer-version
                  verify-operator-reason
                  verify-operator-reasons
                  verify-reason-kind
                  verify-reason-vocabulary
                  verify-rejection-reasons
                  verify-refusal-reasons)
         (prefix-in format: "../extensions/gsd/verify-command-format.rkt"))

;; ============================================================
;; Helpers
;; ============================================================
(define (extraction d)
  (format:verify-declaration-extraction d))
(define (reasons-of d)
  (format:verify-extraction-operator-reasons (extraction d)))
(define (reason-codes d)
  (map (lambda (r) (hash-ref r 'code)) (reasons-of d)))

(define (non-empty-string-value? v)
  (and (string? v) (positive? (string-length v))))

(define (extraction-keys h)
  (sort (for/list ([k (in-list (hash-keys h))])
          (symbol->string k))
        string<?))

;; ============================================================
;; A. Fail-closed shell chain grammar
;; ============================================================
(define grammar-suite
  (test-suite "verify command grammar kernel"

    ;; ----------------------------------------------------------
    ;; fail-closed chain splitting
    ;; ----------------------------------------------------------
    (test-case "legacy single-line contracts pass through as one stage"
      (check-equal? (verify-command-stages "raco make a.rkt") '("raco make a.rkt"))
      (check-equal? (verify-command-stages "  exit 0  ") '("exit 0")))

    (test-case "absent declarations refuse instead of shaping a stage"
      (check-false (verify-command-stages #f))
      (check-false (verify-command-stages ""))
      (check-false (verify-command-stages "   \n  ")))

    (test-case "&& and newline chains split into trimmed document-order stages"
      (check-equal? (verify-command-stages "raco make a.rkt && raco fmt --check a.rkt")
                    '("raco make a.rkt" "raco fmt --check a.rkt"))
      (check-equal? (verify-command-stages "  raco make a.rkt \n  racket tests/a.rkt  \n")
                    '("raco make a.rkt" "racket tests/a.rkt")))

    (test-case "the <project-base> placeholder is a literal token, not a redirect"
      (check-equal? (verify-command-stages "cd <project-base>/q && racket scripts/metrics.rkt --lint")
                    '("cd <project-base>/q" "racket scripts/metrics.rkt --lint")))

    (test-case "ambiguous chain shapes refuse (never a guessed stage list)"
      ;; dangling / doubled delimiters
      (check-false (verify-command-stages "raco make a.rkt &&"))
      (check-false (verify-command-stages "raco make a.rkt && && racket tests/a.rkt"))
      (check-false (verify-command-stages "raco make a.rkt\n\nracket tests/a.rkt"))
      ;; unbalanced quote
      (check-false (verify-command-stages "raco make a.rkt && printf 'unterminated"))
      ;; a quoted delimiter is never a stage boundary (fail closed, not guess)
      (check-false (verify-command-stages "printf 'quoted && delimiter'\nracket tests/a.rkt"))
      ;; shell keywords are never launched as gates
      (check-false (verify-command-stages "if true\nracket tests/a.rkt"))
      (check-false (verify-command-stages "raco make a.rkt\nthen racket tests/a.rkt")))

    (test-case "chain-context metacharacters refuse fail-closed"
      (for ([bad (in-list '("raco make a.rkt && racket tests/a.rkt; rm -rf q"
                            "raco make a.rkt && racket tests/a.rkt | tee out"
                            "raco make a.rkt && racket tests/a.rkt > out.txt"
                            "raco make a.rkt && racket tests/a.rkt < in.txt"
                            "raco make a.rkt && racket $(evil).rkt"
                            "raco make a.rkt && `evil`"
                            "raco make a.rkt && (racket tests/a.rkt)"
                            "raco make a.rkt && racket tests/a.rkt &"
                            "raco make a.rkt && racket tests/a.rkt \\"
                            "raco make a.rkt && racket tests/a.rkt #comment"))])
        (check-false (verify-command-stages bad) (format "must refuse: ~a" bad))))

    ;; ----------------------------------------------------------
    ;; canonical chain composition
    ;; ----------------------------------------------------------
    (test-case "canonical composition keeps the legacy bytes and joins chains"
      ;; single-line legacy contract: byte-for-byte, including whitespace
      (check-equal? (canonical-verify-command "  exit 0  ") "  exit 0  ")
      (check-equal? (canonical-verify-command "racket tests/test-foo.rkt")
                    "racket tests/test-foo.rkt")
      ;; a single-line && chain is returned exactly as authored
      (check-equal? (canonical-verify-command "raco make a.rkt &&   racket tests/a.rkt")
                    "raco make a.rkt &&   racket tests/a.rkt")
      ;; multiline canonicalizes to the audited fail-fast && chain
      (check-equal? (canonical-verify-command "raco make a.rkt\nracket tests/a.rkt")
                    "raco make a.rkt && racket tests/a.rkt")
      ;; refusal yields the empty executable command, never a partial chain
      (check-equal? (canonical-verify-command "raco make a.rkt\n\nracket tests/a.rkt") ""))

    ;; ----------------------------------------------------------
    ;; span vocabulary and classification
    ;; ----------------------------------------------------------
    (test-case "accepted first-token vocabulary covers this repository's toolchain"
      (for ([token (in-list '("racket" "raco"
                                       "bash"
                                       "sh"
                                       "python3"
                                       "pytest"
                                       "npm"
                                       "pnpm"
                                       "yarn"
                                       "make"
                                       "git"
                                       "cd"
                                       "env"
                                       "test"
                                       "printf"))])
        (check-true (and (member token verify-accept-first-tokens) #t)
                    (format "missing accepted token: ~a" token)))
      (check-equal? (verify-first-token "  racket tests/a.rkt") #f)
      (check-equal? (verify-first-token "racket tests/a.rkt") "racket"))

    (test-case "accepted spans keep their pinned canonical forms"
      (for ([span (in-list '("racket tests/a.rkt" "raco fmt --check a.rkt"
                                                  "bash -c 'test -f a.rkt'"
                                                  "test -f q/a.rkt"
                                                  "[ -f q/a.rkt ]"
                                                  "./scripts/run.sh"
                                                  "<project-base>/q/racket.sh"))])
        (define verdict (verify-classify-span span))
        (check-true (and (pair? verdict) (eq? (car verdict) 'accept)) (format "must accept: ~a" span))
        (check-equal? (cdr verdict) span))
      ;; the retained metrics shorthand canonicalizes to the real runner path
      (check-equal? (cdr (verify-classify-span "metrics.rkt --lint"))
                    "racket scripts/metrics.rkt --lint")
      (check-equal? (cdr (verify-classify-span "scripts/metrics.rkt --lint"))
                    "racket scripts/metrics.rkt --lint"))

    (test-case "prose mentions are never command-shaped"
      (for ([span (in-list '("" "unknown" "distinct_*" "R2 PASSED" "q/foo.rkt"))])
        (check-eq? (verify-classify-span span) 'prose (format "must be prose: ~a" span))))

    (test-case "rejected spans carry the named vocabulary (never a silent drop)"
      (check-equal? (verify-classify-span "! grep -q x q/x.txt") (cons 'reject 'inverted-command))
      (check-equal? (verify-classify-span "[[ -f q/x.rkt ]]") (cons 'reject 'bash-only-construct))
      (check-equal? (verify-classify-span "grep -L forbidden q/report.txt")
                    (cons 'reject 'grep-exit-semantics))
      (check-equal? (verify-classify-span "find q -name x") (cons 'reject 'unsupported-command))
      (check-equal? (verify-classify-span "foo | bar") (cons 'reject 'ambiguous-shell-span)))

    (test-case "every classification reason is inside the closed rejection vocabulary"
      (for ([span
             (in-list
              '("! x" "[[ -f x ]]" "grep -q x f" "find q" "foo | bar" "sed -n 1p f" "rg TOKEN"))])
        (define verdict (verify-classify-span span))
        (check-true (and (pair? verdict) (eq? (car verdict) 'reject)))
        (check-true (and (member (cdr verdict) verify-rejection-reasons) #t)
                    (format "~a: reason ~a is outside the vocabulary" span (cdr verdict)))))

    (test-case "bullet decoration detection mirrors the extraction layout rule"
      (check-true (and (verify-decorated? "- `racket tests/a.rkt` green") #t))
      (check-true (and (verify-decorated? "prose\n* `racket a.rkt`") #t))
      (check-false (verify-decorated? "raco make a.rkt")))

    ;; ----------------------------------------------------------
    ;; frozen extraction shape (delegating layout layer)
    ;; ----------------------------------------------------------
    (test-case "the extraction result shape is frozen for existing consumers"
      (check-equal? (extraction-keys (extraction "- `racket tests/a.rkt` green"))
                    '("canonical" "commands" "refusal" "rejections")))

    (test-case "bullet layouts keep the complete ordered command list"
      (define d "- `racket tests/a.rkt`, `raco fmt --check a.rkt` green.")
      (define e (extraction d))
      (check-equal? (hash-ref e 'commands) '("racket tests/a.rkt" "raco fmt --check a.rkt"))
      (check-equal? (hash-ref e 'canonical) "racket tests/a.rkt && raco fmt --check a.rkt")
      (check-equal? (hash-ref e 'rejections) '())
      (check-false (hash-ref e 'refusal)))

    (test-case "one rejected span voids the declaration and keeps its span"
      (define e (extraction "- `racket tests/a.rkt` green\n- `grep -L x q/f.txt` clean\n"))
      (check-equal? (hash-ref e 'canonical) "")
      (check-equal? (hash-ref e 'commands) '())
      (check-equal? (hash-ref e 'rejections)
                    (list (hasheq 'reason 'grep-exit-semantics 'span "grep -L x q/f.txt")))
      (check-false (hash-ref e 'refusal)))

    (test-case "every declaration-shape refusal is named and inside the vocabulary"
      (define shapes
        (list (cons #f 'empty-declaration)
              (cons "" 'empty-declaration)
              (cons "- criterion one.\n- criterion two.\n" 'no-executable-command)
              (cons "raco make a.rkt &&" 'ambiguous-grammar)
              (cons "Run this:\nraco test x.rkt" #f)))
      (for ([pair (in-list shapes)])
        (define e (extraction (car pair)))
        (define refusal (hash-ref e 'refusal))
        (cond
          [(cdr pair)
           (check-equal? refusal (cdr pair))
           (check-true (and (member refusal verify-refusal-reasons) #t))]
          [else
           ;; unfenced prose is a NAMED span rejection, not a shape refusal
           (check-false refusal)
           (check-equal? (hash-ref (car (hash-ref e 'rejections)) 'reason)
                         'prose-in-unfenced-multiline)
           (check-equal? (hash-ref (car (hash-ref e 'rejections)) 'span) "Run this:")])))

    ;; ----------------------------------------------------------
    ;; one grammar binding, re-exported (no duplicate divergent parser)
    ;; ----------------------------------------------------------
    (test-case "verify-command-format re-exports the SAME grammar bindings"
      ;; same binding => same procedure values, not a re-implementation
      (check-eq? format:verify-command-stages verify-command-stages)
      (check-eq? format:canonical-verify-command canonical-verify-command)
      (check-eq? format:verify-classify-span verify-classify-span)
      (check-eq? format:verify-operator-reason verify-operator-reason)
      (check-eq? format:verify-first-token verify-first-token)
      (check-equal? format:verify-normalizer-version verify-normalizer-version)
      (check-equal? format:verify-normalizer-version 2))

    ;; ----------------------------------------------------------
    ;; additive structured operator reasons
    ;; ----------------------------------------------------------
    (test-case "the operator-reason vocabulary is closed and fully structured"
      (check-equal? verify-reason-vocabulary
                    (sort (append verify-rejection-reasons verify-refusal-reasons) symbol<?))
      (for ([code (in-list verify-reason-vocabulary)])
        (define entry (verify-operator-reason code))
        (define where (format "reason ~a" code))
        (check-true (hash? entry) where)
        (check-equal? (hash-ref entry 'code) code where)
        (check-true (and (memq (hash-ref entry 'kind) '(rejection refusal)) #t) where)
        (for ([k (in-list '(summary detail next-action))])
          (check-true (non-empty-string-value? (hash-ref entry k #f))
                      (format "~a: ~a must be a non-empty string" where k))))
      (check-true (and (memq 'empty-declaration verify-refusal-reasons) #t))
      (check-true (and (memq 'no-executable-command verify-refusal-reasons) #t))
      (check-true (and (memq 'ambiguous-grammar verify-refusal-reasons) #t))
      (check-true (and (memq 'grep-exit-semantics verify-rejection-reasons) #t)))

    (test-case "reason kinds and unknown symbols are explicit (never a pass)"
      (check-eq? (verify-reason-kind 'grep-exit-semantics) 'rejection)
      (check-eq? (verify-reason-kind 'ambiguous-grammar) 'refusal)
      (check-false (verify-reason-kind 'not-a-reason))
      (check-false (verify-operator-reason 'not-a-reason))
      (check-false (verify-operator-reason "grep-exit-semantics"))
      (check-false (verify-operator-reason #f)))

    (test-case "accepted declarations have no operator reasons"
      (check-equal? (reasons-of "- `racket tests/a.rkt` green") '())
      (check-equal? (reasons-of "exit 0") '()))

    (test-case "each rejection yields one structured reason with its authored span"
      (define d "- `racket tests/a.rkt` green\n- `grep -L x q/f.txt` clean\n")
      (define reasons (reasons-of d))
      (check-equal? (length reasons) 1)
      (define r (car reasons))
      (check-eq? (hash-ref r 'code) 'grep-exit-semantics)
      (check-eq? (hash-ref r 'kind) 'rejection)
      (check-equal? (hash-ref r 'span) "grep -L x q/f.txt")
      (for ([k (in-list '(summary detail next-action))])
        (check-true (non-empty-string-value? (hash-ref r k #f)))))

    (test-case "shape refusals yield a structured reason without a span"
      (define reasons (reasons-of "- criterion one.\n"))
      (check-equal? (length reasons) 1)
      (check-eq? (hash-ref (car reasons) 'code) 'no-executable-command)
      (check-eq? (hash-ref (car reasons) 'kind) 'refusal)
      (check-false (hash-ref (car reasons) 'span)))

    (test-case "unfenced prose rejects through the same structured channel"
      (check-equal? (reason-codes "Run this:\nraco test x.rkt") '(prose-in-unfenced-multiline)))

    (test-case "operator reasons are additive: the extraction result is unchanged"
      (define d "- `racket tests/a.rkt`\n- `find q -name x`\n")
      (define first (extraction d))
      (define before (extraction-keys first))
      (define reasons (format:verify-extraction-operator-reasons first))
      (define second (extraction d))
      (check-equal? (extraction-keys second) before)
      (check-equal? second first)
      (check-equal? (hash-ref second 'status "") "")
      (check-equal? (length reasons) 1)
      ;; deriving reasons never rewrites the frozen fields
      (check-equal? (hash-ref first 'canonical) "")
      (check-equal? (hash-ref first 'commands) '()))))

(module+ main
  (exit (run-tests grammar-suite)))

(module+ test
  (require rackunit/text-ui)
  (run-tests grammar-suite))
