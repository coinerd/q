#lang racket
;; @covers extensions/gsd/responsibility-inventory.rkt

;; tests/test-gsd-responsibility-inventory.rkt — GSD Responsibility & Effect Inventory fitness
;; @speed fast
;; @suite arch
;; @boundary integration
;;
;; v0.99.87 W2 (#9214): enforces that every GSD module under extensions/gsd/ is
;; classified in the machine-readable inventory (no unclassified module), that
;; the domain vocabulary is closed, and that declared effects are consistent
;; with the actual source (no effect declared but absent; no pure module with
;; undeclared fs/subprocess effects).

(require racket/string
         racket/file
         racket/runtime-path
         rackunit
         rackunit/text-ui
         "../extensions/gsd/responsibility-inventory.rkt")

(define-runtime-path gsd-dir "../extensions/gsd")

(define valid-domains
  '(pure-planning campaign-state
                  transition-logic
                  ui-glue
                  persistence
                  event-projection
                  command-parsing
                  compatibility-facade
                  external-ports))

(define valid-effects
  '(fs-write fs-read
             fs-rename
             fs-delete
             mkdir
             dir-list
             sha256
             git
             subprocess
             parameterize
             make-param
             path-ops
             network
             github
             dynamic-require))

;; Modules that are part of the inventory but not domain modules (the inventory
;; file itself). Excluded from the "every .rkt classified" check.
(define non-domain-files '("responsibility-inventory.rkt"))

;; Scanner false-positive exclusions: (file . (effect ...)) where the token is a
;; symbol/predicate, not a real call (e.g. policy.rkt's 'write-file decision
;; symbol). path-ops for policy.rkt is REAL (string->path/path->string in
;; in-planning-dir?); fs-read for wave-executor.rkt is REAL (call-with-input-file).
(define scanner-exclusions '(("policy.rkt" . (fs-write))))

;; Scan a source file's non-comment code for effect markers.
(define (scan-effects file)
  (define txt (file->string (build-path gsd-dir file)))
  (define code
    (string-join (for/list ([line (in-list (string-split txt "\n"))]
                            #:unless (regexp-match? #rx"^[ \t]*;" line))
                   line)
                 "\n"))
  (define (has? pat)
    (regexp-match? pat code))
  (for/list ([eff (in-list valid-effects)]
             #:when
             (case eff
               [(fs-write) (has? #rx"\\((?:write-file|with-output-to-file|call-with-output-file)")]
               [(fs-read) (has? #rx"call-with-input-file|file->string|file->lines|file->bytes")]
               [(fs-rename) (has? #rx"rename-file-or-directory|copy-file")]
               [(fs-delete) (has? #rx"delete-file|delete-directory")]
               [(mkdir) (has? #rx"make-directory")]
               [(dir-list) (has? #rx"directory-list")]
               [(sha256) (has? #rx"sha256-string")]
               [(git) (has? #rx"\"git\"")]
               [(subprocess) (has? #rx"subprocess|process-wait|process-status")]
               [(parameterize) (has? #rx"parameterize")]
               [(make-param) (has? #rx"make-parameter")]
               [(path-ops) (has? #rx"string->path|path->string")]
               [(network) (has? #rx"net/url|net/http|http-conn")]
               [(github) (has? #rx"github-api|gh api")]
               [(dynamic-require) (has? #rx"dynamic-require")]
               [else #f]))
    eff))

(define inventory-map
  (for/hash ([e (in-list inventory)])
    (values (entry-module-file e) e)))

(define inventory-tests
  (test-suite "gsd-responsibility-inventory"

    (test-case "every GSD module on disk is classified (no unclassified module)"
      (define on-disk
        (filter (lambda (f) (and (string-suffix? f ".rkt") (not (member f non-domain-files))))
                (map path->string (directory-list gsd-dir))))
      (define unclassified (filter (lambda (f) (not (hash-has-key? inventory-map f))) on-disk))
      (check-equal? unclassified '() (format "unclassified GSD modules: ~a" unclassified))
      (check-equal? (length on-disk)
                    (length inventory)
                    "inventory must cover every GSD module exactly once")
      ;; +3 BUG-0077 honest completion (delivery-finalize, campaign-result,
      ;; tracker-reconciliation), on top of journal/receipt/coordinator +3;
      ;; +1 W0 checkpoint; +3 W1 adapter, wiring and resume retry;
      ;; +1 v1.00.33 recovery continuation (delivery-recovery);
      ;; +1 BUG-0079 approved-head publication (delivery-publication);
      ;; +6 automatic review (dispatch, rejection, checkpoint, usage,
      ;; worktree and the extension-owned runtime session adapter);
      ;; +3 BUG-0042 extractions (owned-lane, wave-scope,
      ;; evidence-author-journal); +1 BUG-0086 owner session recovery;
      ;; +10 BUG-0088 governed supplemental evidence (effective-verify-binding,
      ;; supplemental evidence format/authority/journal/proof/publication/
      ;; promotion/controller/operator and the hardened review-tools registry);
      ;; +1 BUG-0088 public promotion audit validator;
      ;; +1 BUG-0088 supplemental worker descriptor/projections.
      ;; +4 fixed disposition policy/claim/store/backend owners.
      ;; +1 BUG-0090 storage/validation core extracted from the journal
      ;; (supplemental-evidence-store-core).
      (check-equal?
       (length inventory)
       105
       "100 existing modules + four disposition owners + BUG-0090 journal store core"))

    (test-case "supplemental worker descriptor projects policy without dispatch authority"
      (define descriptor (hash-ref inventory-map "supplemental-author-contract.rkt"))
      (check-equal? (entry-domain descriptor) 'transition-logic)
      (check-equal? (entry-effects descriptor) '(fs-read make-param))
      (check-equal? (entry-params descriptor) '(current-supplemental-author-contract))
      (for ([dep '("supplemental-evidence-authority" "supplemental-evidence-format"
                                                     "util/worker-tool-policy"
                                                     "../gsd-planning/execution-policy")])
        (check-not-false (member dep (entry-deps descriptor))))
      (check-false (member "wave-runner-port" (entry-deps descriptor))))

    (test-case "public promotion audit validation preserves pure format dependencies"
      (define audit (hash-ref inventory-map "review-audit-promotion.rkt"))
      (check-equal? (entry-domain audit) 'transition-logic)
      (check-equal? (entry-effects audit) '())
      (check-equal? (entry-params audit) '())
      (check-equal? (entry-deps audit) '("supplemental-evidence-format" "evidence-repair-format")))

    (test-case "evidence recovery classification preserves I/O boundaries"
      (define formats '("evidence-repair-format.rkt"))
      ;; Discovery delegates source reads to runner coverage/the probe reader;
      ;; keep direct source-marker checks separate from delegated effects.
      (define io-owners
        '("evidence-probe.rkt" "evidence-readiness.rkt"
                               "evidence-repair-ledger.rkt"
                               "evidence-runner-coverage.rkt"
                               "author-scope.rkt"
                               "red-checkpoint.rkt"))
      (define discovery (hash-ref inventory-map "evidence-discovery.rkt"))
      ;; `dynamic-require` is the rejected-form marker in the helper walk.
      (check-equal? (entry-effects discovery) '(git parameterize path-ops dynamic-require))
      (for ([dep '("evidence-runner-coverage" "scripts/gsd-evidence-probe-runner")])
        (check-not-false (member dep (entry-deps discovery))))
      (for ([file (in-list formats)])
        (check-equal? (entry-effects (hash-ref inventory-map file)) '(sha256)))
      (for ([file (in-list io-owners)])
        (check-not-false (memq 'fs-read (entry-effects (hash-ref inventory-map file))))))

    (test-case "author lifecycle is not classified as pure or a facade"
      (for ([file '("author-scope.rkt" "author-execution.rkt"
                                       "author-checkpoint.rkt"
                                       "evidence-author-repair.rkt"
                                       "red-checkpoint.rkt"
                                       "author-session-recovery.rkt")])
        (check-false (and (memq (entry-domain (hash-ref inventory-map file))
                                '(pure-planning compatibility-facade))
                          #t)))
      (for ([binding '(("evidence-author-repair.rkt" "author-scope" "delivery-journal")
                       ("author-execution.rkt" "red-checkpoint"
                                               "delivery-journal"
                                               "../gsd-planning/execution-policy")
                       ("author-checkpoint.rkt" "campaign-repository" "delivery-receipt")
                       ("red-checkpoint.rkt" "verification-job" "author-scope")
                       ("author-session-recovery.rkt" "delivery-verifier"
                                                      "review-proof"
                                                      "verification-job"
                                                      "system-adapters"
                                                      "owner-correction")
                       ("owner-correction.rkt" "generated-readme"
                                               "review-proof"
                                               "../gsd-planning/execution-policy"))])
        (for ([dep (in-list (cdr binding))])
          (check-not-false (member dep (entry-deps (hash-ref inventory-map (car binding))))))))

    (test-case "domain vocabulary is closed"
      (for ([e (in-list inventory)])
        (check-not-false (memq (entry-domain e) valid-domains)
                         (format "~a: invalid domain ~a" (entry-module-file e) (entry-domain e)))))

    (test-case "effect vocabulary is closed"
      (for ([e (in-list inventory)])
        (for ([eff (in-list (entry-effects e))])
          (check-not-false (memq eff valid-effects)
                           (format "~a: invalid effect ~a" (entry-module-file e) eff)))))

    (test-case "declared effects match actual source (no ghost/absent effects)"
      (for ([e (in-list inventory)])
        (define file (entry-module-file e))
        (define declared (entry-effects e))
        (define measured (scan-effects file))
        (define excluded-pair (assoc file scanner-exclusions))
        (define excluded
          (if excluded-pair
              (cdr excluded-pair)
              '()))
        (define measured-clean
          (if excluded
              (filter (lambda (x) (not (memq x excluded))) measured)
              measured))
        (define missing (filter (lambda (x) (not (memq x declared))) measured-clean))
        (define ghost (filter (lambda (x) (not (memq x measured))) declared))
        (check-equal? missing '() (format "~a: source contains undeclared effects ~a" file missing))
        (check-equal?
         ghost
         '()
         (format "~a: inventory declares effects not present in source: ~a" file ghost))))

    (test-case "module files referenced by inventory exist"
      (for ([e (in-list inventory)])
        (check-true (file-exists? (build-path gsd-dir (entry-module-file e)))
                    (format "~a missing from disk" (entry-module-file e)))))))

(module+ main
  (exit (run-tests inventory-tests)))

(module+ test
  (require rackunit/text-ui)
  (run-tests inventory-tests))
