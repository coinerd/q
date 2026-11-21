#lang racket

;; tests/test-semantic-navigator.rkt — CL2 (#9979) contract pins.
;; The navigator is a DEVELOPER-ONLY navigational aid: these tests pin that it
;; distinguishes static evidence from unresolved edges, never claims a
;; behavioral contract is verified, reuses qt selection, and honestly reports
;; which CL0 comparisons are NOT agent-trial measurements.
;;
;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../scripts/semantic-navigator.rkt")

(define-runtime-path repo-root "..")
(define git-exe (find-executable-path "git"))

(define (write-text path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate/replace))

(define (make-fixture-root)
  (check-true (and git-exe #t) "git is required for the tracked-source index")
  (define root (make-temporary-file "cl2nav-~a" 'directory))
  (parameterize ([current-directory root])
    (system* git-exe "init" "--quiet")
    (write-text (build-path root "pkg" "a.rkt")
                (string-append "#lang racket/base\n(provide alpha shared)\n(define (alpha x) x)\n"
                               "(define shared 1)\n(dynamic-require \"b.rkt\" 'probe)\n"
                               "(define computed-module (string-append \"b\" \".rkt\"))\n"
                               "(dynamic-require computed-module 'probe)\n"))
    (write-text (build-path root "pkg" "b.rkt")
                "#lang racket/base\n(provide shared beta)\n(define shared 2)\n(define beta 3)\n")
    (write-text
     (build-path root "docs" "architecture" "subsystem-manifest.rktd")
     "((schema-version 1) (input-availability ((coverage-manifest . \"required\"))) (subsystems (((name \"pkg\") (production-modules 2)))))")
    (write-text (build-path root "docs" "architecture" "subsystem-maps" "pkg.md")
                "Map excerpt: `pkg/a.rkt` is the entry point.\n")
    (write-text (build-path root "tests" ".coverage-manifest.json") "{\"entries\":[]}")
    (write-text (build-path root "tests" ".quick-admission.rktd")
                "((record-version . 1) (owners ()))")
    (system* git-exe "add" "-A"))
  root)

(define (has-fact? facts symbol)
  (and (member symbol facts) #t))

(test-case "ambiguous symbol reports every static definition and a named uncertainty"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (define result (symbol-query 'shared #:root root))
                  (check-equal? (length (hash-ref result 'definitions)) 2)
                  (check-equal? (sort (map (lambda (d) (hash-ref d 'module))
                                           (hash-ref result 'definitions))
                                      string<?)
                                '("pkg/a.rkt" "pkg/b.rkt"))
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'ambiguous-symbol)))
                (lambda () (delete-directory/files root))))

(test-case "missing symbol fails soft with no invented definition"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (define result (symbol-query 'definitely-not-a-q-symbol #:root root))
                  (check-equal? (hash-ref result 'definitions) '())
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'no-static-definition)))
                (lambda () (delete-directory/files root))))

(test-case "dynamic requires distinguish literal, computed and static edges"
  (define root (make-fixture-root))
  (dynamic-wind
   void
   (lambda ()
     (define result (module-query "pkg/a.rkt" #:root root))
     (define dynamic (hash-ref result 'dynamic-requires))
     (define (statuses)
       (map (lambda (d) (hash-ref d 'status)) dynamic))
     (check-true (and (member 'literal-resolved (statuses)) #t))
     (check-true (and (member 'unresolved (statuses)) #t))
     (check-true (has-fact? (hash-ref result 'uncertainty) 'unresolved-dynamic-target))
     (check-equal?
      (hash-ref (first (filter (lambda (d) (equal? (hash-ref d 'status) 'literal-resolved)) dynamic))
                'module)
      "pkg/b.rkt")
     ;; A dynamic edge is not a static caller edge.
     (check-false (member "pkg/a.rkt"
                          (map (lambda (c) (hash-ref c 'module))
                               (hash-ref (module-query "pkg/b.rkt" #:root root) 'callers)))))
   (lambda () (delete-directory/files root))))

(test-case "missing curated map marks the navigational index stale"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (delete-file (build-path root "docs" "architecture" "subsystem-maps" "pkg.md"))
                  (define result (module-query "pkg/a.rkt" #:root root))
                  (check-true (hash-ref (hash-ref result 'freshness) 'stale?))
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'stale-map-metadata)))
                (lambda () (delete-directory/files root))))

(test-case "ownership gap is explicit and never presented as verified coverage"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (define result (module-query "pkg/b.rkt" #:root root))
                  (check-equal? (hash-ref result 'owner-tests) '())
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'no-owner-tests))
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'no-curated-map)))
                (lambda () (delete-directory/files root))))

(test-case "map excerpts carry provenance but are not behavioral verification"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (define result (module-query "pkg/a.rkt" #:root root))
                  (define excerpts (hash-ref result 'map-excerpts))
                  (check-equal? (length excerpts) 1)
                  (check-true (string-contains? (hash-ref (first excerpts) 'text) "pkg/a.rkt"))
                  (check-equal? (hash-ref result 'verification-note)
                                "navigational evidence only; not behavioral verification"))
                (lambda () (delete-directory/files root))))

(test-case "focused context reuses qt selection and is labelled reproduction only"
  (define result (focused-context '("agent/state.rkt") #:root repo-root))
  (check-true (and (member "tests/test-agent-state.rkt" (hash-ref result 'selected-owners)) #t))
  (check-true (and (member "tests/test-agent-state.rkt"
                           (map (lambda (t) (hash-ref t 'test)) (hash-ref result 'owner-tests)))
                   #t))
  (check-true (string-contains? (hash-ref result 'verification-note) "not PR verification"))
  (check-true (pair? (hash-ref result 'provenance)))
  (check-false (hash-ref (hash-ref result 'freshness) 'stale?)))

(test-case "CL0 frozen navigation facts are mechanically contained in bundles"
  (define report (benchmark-report #:root repo-root))
  (for ([task (in-list (hash-ref report 'tasks))])
    (check-true (hash-ref task 'contained)
                (format "~a missing ~a" (hash-ref task 'task) (hash-ref task 'missing))))
  (check-equal? (sort (map (lambda (t) (hash-ref t 'task)) (hash-ref report 'tasks)) string<?)
                '("T1" "T3" "T4"))
  (check-true (string-contains? (hash-ref report 'limitation) "PENDING"))
  (check-false (hash-ref report 'agent-trial-measurement)))

(test-case "an empty tracked-source set fails closed instead of yielding an empty index"
  (define root (make-temporary-file "cl2nav-empty-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-directory root])
                    (system* git-exe "init" "--quiet"))
                  (check-exn exn:fail? (lambda () (build-index #:root root))))
                (lambda () (delete-directory/files root))))

(test-case "computed dynamic-require targets are resolved through define-runtime-path"
  (define result
    (module-query "extensions/gsd/supplemental-evidence-controller.rkt" #:root repo-root))
  (define dynamic (hash-ref result 'dynamic-requires))
  (check-true (for/or ([d (in-list dynamic)])
                (and (eq? (hash-ref d 'status) 'runtime-path-resolved)
                     (equal? (hash-ref d 'module)
                             "extensions/gsd/supplemental-evidence-disposition.rkt"))))
  (check-false (has-fact? (hash-ref result 'uncertainty) 'unresolved-dynamic-target))
  (check-true (hash-ref result 'manifest-dynamic-marker?)))

(test-case "cache reuse is gated by source identity, not only repository root"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (check-true (and (hash-ref (symbol-query 'alpha #:root root) 'definitions) #t))
                  (write-text (build-path root "pkg" "a.rkt")
                              "#lang racket/base\n(provide gamma)\n(define gamma 7)\n")
                  (define after (symbol-query 'gamma #:root root))
                  (check-equal? (map (lambda (d) (hash-ref d 'module)) (hash-ref after 'definitions))
                                '("pkg/a.rkt"))
                  (check-equal? (hash-ref (symbol-query 'alpha #:root root) 'definitions) '()))
                (lambda () (delete-directory/files root))))

(test-case "unavailable coverage metadata is not reported as absent ownership"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (delete-file (build-path root "tests" ".coverage-manifest.json"))
                  (define missing (module-query "pkg/b.rkt" #:root root))
                  (check-true (has-fact? (hash-ref missing 'uncertainty) 'coverage-unavailable))
                  (check-false (has-fact? (hash-ref missing 'uncertainty) 'no-owner-tests))
                  (write-text (build-path root "tests" ".coverage-manifest.json") "{not json")
                  (define corrupt (module-query "pkg/b.rkt" #:root root))
                  (check-true (has-fact? (hash-ref corrupt 'uncertainty) 'coverage-unavailable))
                  (check-false (has-fact? (hash-ref corrupt 'uncertainty) 'no-owner-tests)))
                (lambda () (delete-directory/files root))))

(test-case "reader failures preserve partial evidence and mark the index incomplete"
  (define root (make-fixture-root))
  (dynamic-wind
   void
   (lambda ()
     (write-text (build-path root "pkg" "a.rkt")
                 "#lang racket/base\n(provide alpha)\n(define alpha 1\n")
     (define result (module-query "pkg/a.rkt" #:root root))
     (check-true (has-fact? (hash-ref result 'uncertainty) 'source-parse-incomplete))
     (check-true (string? (hash-ref result 'source-parse-error)))
     (check-true
      (and (member "alpha" (map (lambda (e) (hash-ref e 'name)) (hash-ref result 'exports))) #t)))
   (lambda () (delete-directory/files root))))

(test-case "generated struct accessors are discoverable as exports"
  (define result (symbol-query 'campaign-decision-kind #:root repo-root))
  (check-true (and (member "extensions/gsd/campaign-decision.rkt"
                           (map (lambda (d) (hash-ref d 'module)) (hash-ref result 'definitions)))
                   #t))
  (check-true (for/or ([d (in-list (hash-ref result 'definitions))])
                (equal? (hash-ref d 'kind) "generated-struct-accessor"))))

(test-case "freshness does not claim regeneration was verified"
  (define freshness (hash-ref (module-query "agent/state.rkt" #:root repo-root) 'freshness))
  (check-false (hash-ref freshness 'regeneration-verified))
  (check-true (string-contains? (hash-ref freshness 'stale-semantics) "does not assert"))
  (check-true (hash-has-key? (hash-ref freshness 'source-identity) 'digest)))
