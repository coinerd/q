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
    (write-text
     (build-path root "pkg" "a.rkt")
     "#lang racket/base\n(provide alpha shared)\n(define (alpha x) x)\n(define shared 1)\n(dynamic-require \"b.rkt\" 'probe)\n")
    (write-text (build-path root "pkg" "b.rkt")
                "#lang racket/base\n(provide shared beta)\n(define shared 2)\n(define beta 3)\n")
    (write-text
     (build-path root "docs" "architecture" "subsystem-manifest.rktd")
     "((schema-version 1) (input-availability ((coverage-manifest . \"required\"))) (subsystems ((name \"pkg\") (production-modules 2))))")
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

(test-case "dynamic requires are reported as unresolved, not as static callers"
  (define root (make-fixture-root))
  (dynamic-wind void
                (lambda ()
                  (define result (module-query "pkg/a.rkt" #:root root))
                  (define dynamic (hash-ref result 'dynamic-requires))
                  (check-equal? (map (lambda (d) (hash-ref d 'module)) dynamic) '("pkg/b.rkt"))
                  (check-equal? (map (lambda (d) (hash-ref d 'status)) dynamic) '(unresolved))
                  (check-true (has-fact? (hash-ref result 'uncertainty) 'dynamic-require))
                  ;; A dynamic edge is not a static caller edge.
                  (check-false (member "pkg/a.rkt"
                                       (map (lambda (c) (hash-ref c 'module))
                                            (hash-ref (module-query "pkg/b.rkt" #:root root)
                                                      'callers)))))
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
