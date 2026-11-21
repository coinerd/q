#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-subsystem-maps.rkt — CL1 subsystem-map pins (#9978)
;;
;;   1. Freshness: the committed manifest regenerates byte-identically from
;;      existing metadata (tests/.coverage-manifest.json,
;;      tests/.quick-admission.rktd, reports/test-ownership-map.json,
;;      extensions/gsd/responsibility-inventory.rkt,
;;      docs/architecture/dependency-policy.rktd, working-tree source).
;;   2. Manifest structure: every subsystem record carries the full columns.
;;   3. Curated maps reference only files that exist: every `.rkt`-suffixed
;;      path in backticks inside docs/architecture/subsystem-maps/*.md exists.
;;   4. CL0 frozen benchmark tasks are answerable from the maps alone (the
;;      "fewer reads" demonstration): T1/T3/T4 required facts appear verbatim.
;;   5. Dynamic-dependency honesty: modules the manifest marks with
;;      dynamic-require are called out in the curated maps.

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/path
         racket/string
         "../scripts/subsystem-manifest.rkt")

(define-runtime-path manifest-path "../docs/architecture/subsystem-manifest.rktd")
(define-runtime-path maps-dir "../docs/architecture/subsystem-maps")

;; raco test does not guarantee the repo root as CWD: anchor every relative
;; path to this file's location (same pattern as tests/test-milestone-gate.rkt).
(define repo-root (simplify-path (build-path (path-only manifest-path) ".." "..")))

(define committed (string-trim (file->string manifest-path)))
(define rebuilt (manifest->string (build-manifest repo-root)))

(define (entry-ref* entry key [default #f])
  (define found (assoc key entry))
  (if (not found)
      default
      (let ([rest (cdr found)])
        (cond
          [(null? rest) default]
          [(and (pair? rest) (null? (cdr rest))) (car rest)]
          [else rest]))))

(define (subsystem datum name)
  (for/first ([s (in-list (entry-ref* datum 'subsystems))]
              #:when (string=? (entry-ref* s 'name "") name))
    s))

(define (map-files)
  (sort (for/list ([p (in-directory maps-dir)]
                   #:when (string-suffix? (path->string p) ".md"))
          p)
        path<?))

(define (map-text)
  (string-join (map file->string (map-files)) "\n"))

(module+ test
  (test-case "committed manifest regenerates byte-identically from existing metadata"
    (check-equal? committed rebuilt)
    (check-true (non-empty-string? committed)))

  (test-case "every manifest subsystem record carries the full column set"
    (define datum (call-with-input-file manifest-path read))
    (define columns
      '(name production-modules
             provide-total
             dynamic-require-modules
             owner-tests
             coverage-mappings
             outbound-edges
             gsd-inventory
             policy-exceptions))
    (check-false (null? (entry-ref* datum 'subsystems)))
    (for ([s (in-list (entry-ref* datum 'subsystems))])
      (for ([column (in-list columns)])
        (check-true (and (assoc column s) #t)
                    (format "subsystem ~a missing column ~a" (entry-ref* s 'name) column)))
      (check-true (exact-nonnegative-integer? (entry-ref* s 'production-modules)))
      (check-true (exact-nonnegative-integer? (entry-ref* s 'provide-total))))
    ;; spot-checks against known CL0 measurements (structure, not volatile counts)
    (check-equal? (entry-ref* (subsystem datum "extensions") 'name) "extensions")
    (check-equal? (entry-ref* (subsystem datum "llm") 'name) "llm")
    (check-true (if (member "extensions/gsd/supplemental-evidence-controller.rkt"
                            (entry-ref* (subsystem datum "extensions") 'dynamic-require-modules))
                    #t
                    #f)
                "supplemental-evidence-controller is a known dynamic-require module"))

  (test-case "curated maps reference only files that exist"
    (for ([p (in-list (map-files))])
      (check-true (file-exists? p) (format "map file exists: ~a" p)))
    (define text (map-text))
    (define referenced
      (remove-duplicates
       (regexp-match* #rx"`([A-Za-z0-9][A-Za-z0-9/_.-]*\\.rkt)`" text #:match-select cadr)
       string=?))
    (check-true (pair? referenced) "the maps name concrete modules")
    (for ([path (in-list referenced)])
      (check-true (file-exists? (build-path repo-root path))
                  (format "referenced module exists: ~a" path))))

  (test-case "T1 (campaign decision) is answerable from gsd-campaign.md alone"
    (define text (file->string (build-path maps-dir "gsd-campaign.md")))
    (for ([token (in-list '("extensions/gsd/campaign-decision.rkt"
                            "extensions/gsd/go-orchestrator.rkt"
                            "extensions/gsd/campaign-state.rkt"
                            "extensions/gsd/campaign-repository.rkt"
                            "decide-campaign-action"
                            "campaign-complete"
                            "tests/test-gsd-campaign-decision.rkt"))])
      (check-true (string-contains? text token) (format "T1 fact in map: ~a" token))))

  (test-case "T2 (delivery outcome) ownership is answerable from gsd-campaign.md"
    (define text (file->string (build-path maps-dir "gsd-campaign.md")))
    (for ([token (in-list '("extensions/gsd/delivery-outcome.rkt"
                            "tests/test-gsd-delivery-stop-decision.rkt"))])
      (check-true (string-contains? text token) (format "T2 fact in map: ~a" token))))

  (test-case "T3 (agent/state impact hub) is answerable from agent-session.md"
    (define text (file->string (build-path maps-dir "agent-session.md")))
    (for ([token (in-list '("agent/state.rkt" "agent/loop.rkt"
                                              "runtime/agent-session.rkt"
                                              "runtime/session/session-store.rkt"
                                              "tests/test-agent-state.rkt"
                                              "fan-out 0"))])
      (check-true (string-contains? text token) (format "T3 fact in map: ~a" token))))

  (test-case "T4 (provider retry ownership) is answerable from provider.md"
    (define text (file->string (build-path maps-dir "provider.md")))
    (for ([token (in-list '("llm/provider-errors.rkt" "runtime/provider-retry.rkt"
                                                      "runtime/iteration/retry-policy.rkt"
                                                      "tests/test-retry-error-classification.rkt"
                                                      "held-request"))])
      (check-true (string-contains? text token) (format "T4 fact in map: ~a" token))))

  (test-case "T5 (width truncation) ownership is answerable from the maps"
    (define text (map-text))
    (for ([token (in-list '("tui/char-width.rkt" "tests/tui/test-char-width.rkt"))])
      (check-true (string-contains? text token) (format "T5 fact in maps: ~a" token))))

  (test-case "dynamic dependencies are called out where the manifest marks them"
    (define supplemental-text (file->string (build-path maps-dir "gsd-supplemental-evidence.md")))
    (check-true (string-contains? supplemental-text "dynamic-require")
                "supplemental-evidence map marks its unresolved dynamic edges")
    (check-true (string-contains? supplemental-text "supplemental-evidence-controller.rkt")
                "supplemental-evidence map names the controller"))

  (test-case "optional inputs are recorded, never silently omitted"
    (define datum (call-with-input-file manifest-path read))
    (define availability (entry-ref* datum 'input-availability))
    (check-true (pair? availability) "input-availability record present")
    (for ([record (in-list availability)])
      (define status (cdr record))
      (check-true (and (string? status) (non-empty-string? status))
                  (format "input ~a has an explicit status" (car record)))
      (check-false (string-contains? status "unavailable")
                   (format "input ~a is available in this repository" (car record)))))

  (test-case "generation fails loudly when a required input is missing or corrupt"
    (check-true (and (find-executable-path "git") #t))
    (define (expect-failure root pattern)
      (define message
        (with-handlers ([exn:fail? exn-message])
          (build-manifest root)
          #f))
      (check-true (and message (regexp-match? pattern message))
                  (format "generation failed with ~a (got: ~a)" pattern message)))
    (define (make-fixture-root)
      (define root (make-temporary-file "cl1fixture-~a" 'directory))
      (parameterize ([current-directory root])
        (system* (find-executable-path "git") "init" "--quiet")
        (make-directory* (build-path root "pkg"))
        (make-directory* (build-path root "tests"))
        (make-directory* (build-path root "docs" "architecture"))
        (call-with-output-file (build-path root "pkg" "a.rkt")
                               (lambda (out) (display "#lang racket/base\n(provide alpha)\n" out)))
        (call-with-output-file (build-path root "tests" "test-a.rkt")
                               (lambda (out) (display "#lang racket\n" out)))
        (call-with-output-file
         (build-path root "tests" ".coverage-manifest.json")
         (lambda (out)
           (display "{\"entries\":[{\"test\":\"tests/test-a.rkt\",\"covers\":[\"pkg/a.rkt\"]}]}"
                    out)))
        (call-with-output-file
         (build-path root "tests" ".quick-admission.rktd")
         (lambda (out)
           (writeln '((record-version . 1)
                      (owners ("tests/test-a.rkt" (status . admitted)
                                                  (capability . #f)
                                                  (hit-file . "pkg/a.rkt")
                                                  (closure "tests/test-a.rkt" "pkg/a.rkt")
                                                  (hashes ("tests/test-a.rkt" . "abc")))))
                    out)))
        (call-with-output-file (build-path root "docs" "architecture" "dependency-policy.rktd")
                               (lambda (out) (display "((known-exceptions))" out)))
        (system* (find-executable-path "git") "add" "-A"))
      root)

    ;; happy path: valid fixture builds; optional inputs marked unavailable
    (define root (make-fixture-root))
    (dynamic-wind
     void
     (lambda ()
       (define manifest (build-manifest root))
       (check-equal? (entry-ref* manifest 'production-modules) 1)
       (define availability (entry-ref* manifest 'input-availability))
       (check-true (and (assoc 'gsd-inventory availability) #t) "optional gsd inventory recorded")
       (check-true (string-contains? (cdr (assoc 'gsd-inventory availability)) "unavailable")
                   "missing optional gsd inventory is marked unavailable, not omitted")
       (check-true (and (assoc 'ownership-map availability) #t) "optional ownership map recorded")
       ;; required input removed -> explicit failure naming the input
       (delete-file (build-path root "tests" ".coverage-manifest.json"))
       (expect-failure root #rx"required input missing")
       ;; required input corrupt -> explicit failure
       (call-with-output-file (build-path root "tests" ".coverage-manifest.json")
                              (lambda (out) (display "{not json" out))
                              #:exists 'truncate/replace)
       (expect-failure root #rx"unparseable")
       ;; relationship violation: coverage target no longer exists
       (call-with-output-file
        (build-path root "tests" ".coverage-manifest.json")
        (lambda (out)
          (display "{\"entries\":[{\"test\":\"tests/test-a.rkt\",\"covers\":[\"pkg/gone.rkt\"]}]}"
                   out))
        #:exists 'truncate/replace)
       (expect-failure root #rx"coverage target missing on disk")
       ;; restore valid coverage, then empty the admission record -> explicit failure
       (call-with-output-file
        (build-path root "tests" ".coverage-manifest.json")
        (lambda (out)
          (display "{\"entries\":[{\"test\":\"tests/test-a.rkt\",\"covers\":[\"pkg/a.rkt\"]}]}" out))
        #:exists 'truncate/replace)
       (call-with-output-file (build-path root "tests" ".quick-admission.rktd")
                              (lambda (out) (writeln '((record-version . 1) (owners ())) out))
                              #:exists 'truncate/replace)
       (expect-failure root #rx"no audited owners"))
     (lambda () (delete-directory/files root)))

    ;; tracked source deleted from disk -> explicit failure
    (define root2 (make-fixture-root))
    (dynamic-wind void
                  (lambda ()
                    (delete-file (build-path root2 "pkg" "a.rkt"))
                    (expect-failure root2 #rx"cannot be read"))
                  (lambda () (delete-directory/files root2)))))
