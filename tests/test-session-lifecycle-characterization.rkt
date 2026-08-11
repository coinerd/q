#lang racket/base

;; @speed fast
;; @suite arch

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define ledger-path (build-path root "docs" "architecture" "session-lifecycle-trace-v0.99.92.rktd"))

(define expected-paths '(normal error cancel close retry compaction))
(define expected-responsibilities '(orchestration pure-preparation persistence eventing fsm wiring))
(define expected-exceptional-exits
  '(closed-session-guard prompt-contention
                         begin-turn-failure
                         outer-turn-start-failure
                         input-hook-failure
                         context-or-user-persist-failure
                         compaction-failure
                         model-select-or-tracer-start-failure
                         provider-or-iteration-failure
                         dispatch-error-handler-failure
                         index-rebuild-failure
                         rollback-save-back-failure
                         finish-turn-failure
                         release-prompt-failure
                         break-or-kill
                         close-during-active-prompt
                         repeated-close
                         retry-exhausted-with-partial
                         manual-compaction-failure))

(define (read-one path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (check-true (eof-object? (read in))
                                      "lifecycle ledger must contain exactly one datum")
                          datum)))

(define (locator-parts locator)
  (string-split locator ":" #:trim? #f))

(define (check-locator id locator)
  (check-true (string? locator) (format "~a locator must be a string" id))
  (define parts (locator-parts locator))
  (define path (build-path root (car parts)))
  (check-true (file-exists? path) (format "~a evidence file absent: ~a" id locator))
  (when (pair? (cdr parts))
    (define anchor (string-join (cdr parts) ":"))
    (check-true (string-contains? (file->string path) anchor)
                (format "~a evidence anchor absent: ~a" id locator))))

(define (exact-id-bijection label expected entries #:key [key 'id])
  (define ids (map (lambda (entry) (hash-ref entry key)) entries))
  (check-equal? (sort ids symbol<?) (sort expected symbol<?) label)
  (check-equal? (length ids) (length (remove-duplicates ids)) (format "~a IDs must be unique" label)))

(test-case "W0-1: ledger freezes the release baseline and evidence-only scope"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'schema-version) 1)
  (check-eq? (hash-ref ledger 'milestone) 'v0.99.92)
  (check-eq? (hash-ref ledger 'wave) 'W0)
  (check-equal? (hash-ref ledger 'baseline) "a4b85569ff0dbe7971c3fec12babdb3fccbdd329")
  (check-eq? (hash-ref ledger 'scope) 'characterization-only)
  (check-false (hash-ref ledger 'production-change)))

(test-case "W0-2: all six lifecycle paths have exact, ordered, source-anchored effects"
  (define paths (hash-ref (read-one ledger-path) 'paths))
  (exact-id-bijection "lifecycle paths" expected-paths paths)
  (for ([path (in-list paths)])
    (define id (hash-ref path 'id))
    (define effects (hash-ref path 'ordered-effects))
    (check-true (>= (length effects) 6) (format "~a trace is non-vacuous" id))
    (check-equal? (map (lambda (effect) (hash-ref effect 'n)) effects)
                  (range 1 (add1 (length effects)))
                  (format "~a effects must be contiguous and ordered" id))
    (for ([effect (in-list effects)])
      (check-true (symbol? (hash-ref effect 'effect)))
      (check-locator id (hash-ref effect 'anchor)))))

(test-case "W0-3: responsibility and consumer map covers the frozen taxonomy"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'responsibility-taxonomy) expected-responsibilities)
  (define units (hash-ref ledger 'units))
  (check-true (>= (length units) 10))
  (define observed
    (remove-duplicates (append* (map (lambda (unit) (hash-ref unit 'responsibilities)) units))))
  (check-equal? (sort observed symbol<?) (sort expected-responsibilities symbol<?))
  (for ([unit (in-list units)])
    (define id (hash-ref unit 'id))
    (check-true (pair? (hash-ref unit 'responsibilities)) (format "~a needs responsibility" id))
    (check-true (pair? (hash-ref unit 'consumers)) (format "~a needs consumers" id))
    (check-locator id (hash-ref unit 'owner))))

(test-case "W0-4: every exceptional exit has phase, cleanup, terminal, disposition and source"
  (define exits (hash-ref (read-one ledger-path) 'exceptional-exits))
  (exact-id-bijection "exceptional exits" expected-exceptional-exits exits)
  (for ([exit (in-list exits)])
    (define id (hash-ref exit 'id))
    (for ([key (in-list '(phase cleanup terminal classification anchor))])
      (check-true (hash-has-key? exit key) (format "~a lacks ~a" id key)))
    (check-not-false (member (hash-ref exit 'classification) '(IN_SCOPE DEFERRED SEPARATE_MILESTONE)))
    (check-locator id (hash-ref exit 'anchor))))

(test-case "W0-5: parameter scopes pin exceptional rollback save-back timing"
  (define scopes (hash-ref (read-one ledger-path) 'parameter-scopes))
  (exact-id-bijection "parameter scopes"
                      '(current-prompt-operation-session current-rollback-state)
                      scopes
                      #:key 'parameter)
  (define rollback
    (findf (lambda (scope) (eq? (hash-ref scope 'parameter) 'current-rollback-state)) scopes))
  (check-eq? (hash-ref rollback 'save-back) 'rollback-dynamic-wind-after)
  (check-eq? (hash-ref rollback 'unwind) 'after-save-back)
  (for ([scope (in-list scopes)])
    (check-locator (hash-ref scope 'parameter) (hash-ref scope 'anchor))))

(test-case "W0-6: observed defects are explicitly classified, never silently repaired"
  (define findings (hash-ref (read-one ledger-path) 'findings))
  (check-equal? (map (lambda (finding) (hash-ref finding 'id)) findings)
                '(W0-F1 W0-F2 W0-F3 W0-F4 W0-F5))
  (for ([finding (in-list findings)])
    (check-not-false (member (hash-ref finding 'classification)
                             '(IN_SCOPE DEFERRED SEPARATE_MILESTONE)))
    (check-true (string? (hash-ref finding 'summary)))))
