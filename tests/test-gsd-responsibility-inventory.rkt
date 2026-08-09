#lang racket/base

;; tests/test-gsd-responsibility-inventory.rkt — GSD Responsibility & Effect Inventory fitness
;; @speed fast
;; @suite arch
;;
;; v0.99.87 W2 (#9214): enforces that every GSD module under extensions/gsd/ is
;; classified in the machine-readable inventory (no unclassified module), that
;; the domain vocabulary is closed, and that declared effects are consistent
;; with the actual source (no effect declared but absent; no pure module with
;; undeclared fs/subprocess effects).

(require racket/string
         racket/file
         rackunit
         rackunit/text-ui
         "../extensions/gsd/responsibility-inventory.rkt")

(define gsd-dir "extensions/gsd")

(define valid-domains
  '(pure-planning campaign-state
                  transition-logic
                  ui-glue
                  persistence
                  event-projection
                  command-parsing
                  compatibility-facade))

(define valid-effects
  '(fs-write fs-rename
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
;; symbol and path-normalization helpers).
(define scanner-exclusions '(("policy.rkt" . (fs-write path-ops))))

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
      (check-equal? (length inventory) 26 "GSD module count is stable at 26"))

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
