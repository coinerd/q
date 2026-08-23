#lang racket

;; @speed fast  ;; @suite arch

;; BOUNDARY: integration

;; tests/test-arch-parameters.rkt — Architecture regression tests protecting
;; the maintainability-campaign invariants (v0.99.87, PR #9209+).
;;
;; Protects:
;;   1. Agent → Runtime boundary: agent/iteration/ must not acquire NEW
;;      runtime/ dependencies beyond the documented exceptions in
;;      dependency-policy.rktd's agent-iteration-boundary section.
;; @boundary unit
;;   2. session-config: agent/iteration/ must never import the concrete
;;      runtime/session/session-config.rkt implementation. Only the neutral
;;      opaque type predicate (util/types/session-config.rkt) is allowed.
;;   3. Dynamic-parameter audit: every production make-parameter must declare
;;      its semantic lifetime in docs/architecture/parameter-inventory.rktd.
;;      New parameters fail CI until classified. This makes parameter lifetime
;;      an explicit architectural decision rather than a silent side channel.
;;
;; Data source: docs/architecture/dependency-policy.rktd,
;;              docs/architecture/parameter-inventory.rktd
;;
;; Refs: architecture-campaign (PRs #9194–#9209)

(require rackunit
         rackunit/text-ui
         racket/string
         racket/path
         "helpers/arch-utils.rkt")

;; ============================================================
;; Policy + inventory loading
;; ============================================================

(define policy-path (build-path q-dir "docs" "architecture" "dependency-policy.rktd"))
(define policy (call-with-input-file policy-path read))

(define (policy-ref section . keys)
  (let loop ([data (cdr (assoc section policy))]
             [ks keys])
    (if (null? ks)
        data
        (loop (cdr (assoc (car ks) data)) (cdr ks)))))

(define inventory-path (build-path q-dir "docs" "architecture" "parameter-inventory.rktd"))
(define inventory (call-with-input-file inventory-path read))

;; ============================================================
;; Helpers
;; ============================================================

(define valid-categories
  '(CONFIGURATION TURN_LOCAL ITERATION_LOCAL PROMPT_LOCAL SERVICE_HANDLE OTHER_REVIEWED))

;; Flatten ((module (param . category) ...) ...) → ((module param category) ...)
(define (inventory->entries inv)
  (for*/list ([file-entry (in-list inv)]
              [param-entry (in-list (cdr file-entry))])
    (list (car file-entry) (car param-entry) (cdr param-entry))))

;; Application layers under parameter-audit scope. scripts/ (CI/build tooling,
;; separate processes) and tests/ (test-only helpers) are deliberately excluded.
(define production-param-dirs
  '("agent" "runtime"
            "llm"
            "tools"
            "tui"
            "wiring"
            "interfaces"
            "util"
            "extensions"
            "sandbox"
            "cli"
            "gui"))

(define (all-production-rkt-files)
  (append* (for/list ([d (in-list production-param-dirs)])
             (rkt-files-in-recursive d))))

;; Cheap pre-filter: only parse files that mention make-parameter.
(define (has-make-parameter? filepath)
  (regexp-match? #rx"make-parameter" (file->string filepath)))

;; Discover (module . param) pairs for a file using read-based parsing.
(define (discover-parameters filepath)
  (define rel (path->string (find-relative-path (simplify-path q-dir) (simplify-path filepath))))
  (for/list ([p (in-list (extract-parameters filepath))])
    (list rel p)))

;; Map a require path string to its canonical runtime destination, e.g.
;; "../../runtime/session/session-types.rkt" → "runtime/session/session-types.rkt".
(define (runtime-path->destination path)
  (define m (regexp-match #rx"runtime/.*\\.rkt" path))
  (if m
      (car m)
      path))

(define (exc-field entry key)
  (cond
    [(assoc key entry)
     =>
     cdr]
    [else #f]))

;; ============================================================
;; Parameter inventory tests
;; ============================================================

(define parameter-inventory-tests
  (test-suite "architecture-parameter-inventory"

    (test-case "parameter-inventory.rktd is valid and loadable"
      (check-true (file-exists? inventory-path)
                  "docs/architecture/parameter-inventory.rktd must exist")
      (check-true (list? inventory) "parameter-inventory must be a list")
      (define errors
        (for/list ([file-entry (in-list inventory)])
          (cond
            [(not (and (pair? file-entry) (string? (car file-entry))))
             "entry must be (module-path (param . category) ...)"]
            [(not (and (pair? (cdr file-entry)) (list? (cdr file-entry))))
             "entries must be a list of (param . category) pairs"]
            [else
             (define file-errors
               (for/list ([pe (in-list (cdr file-entry))])
                 (cond
                   [(not (and (pair? pe) (symbol? (car pe)) (memq (cdr pe) valid-categories)))
                    (format
                     "~a: bad entry ~a — parameter must be a symbol, category must be one of ~a"
                     (car file-entry)
                     pe
                     valid-categories)]
                   [else #f])))
             (filter identity file-errors)])))
      (define actual-errors (filter identity (append* errors)))
      (check-equal? actual-errors
                    '()
                    (format "Invalid parameter-inventory entries: ~a" actual-errors)))

    (test-case "Every production make-parameter is in the audited inventory"
      (define inventory-entries (inventory->entries inventory))
      (define inventory-keys
        (for/set ([e (in-list inventory-entries)])
          (cons (car e) (symbol->string (cadr e)))))
      (define missing
        (for*/list ([f (in-list (all-production-rkt-files))]
                    #:when (has-make-parameter? f)
                    [entry (in-list (discover-parameters f))]
                    #:when (not (set-member? inventory-keys
                                             (cons (car entry) (symbol->string (cadr entry))))))
          entry))
      (check-equal?
       missing
       '()
       (if (null? missing)
           "all production parameters are audited"
           (format
            (string-append
             "New dynamic parameter ~a found in ~a.~n~n"
             "Dynamic parameters must declare their semantic lifetime in the architecture parameter inventory "
             "(docs/architecture/parameter-inventory.rktd).~n~n"
             "If this state must persist across turns for one session, it probably belongs in lifecycle-state "
             "instead of a process-global parameter.~n~n"
             "Add an entry: (~a ~a CATEGORY) where CATEGORY is one of ~a.~n")
            (if (pair? missing)
                (cadar missing)
                "?")
            (if (pair? missing)
                (caar missing)
                "?")
            (if (pair? missing)
                (caar missing)
                "?")
            (if (pair? missing)
                (cadar missing)
                "?")
            valid-categories))))

    (test-case "Every inventory entry still exists in production"
      ;; A stale entry means a parameter was removed — the inventory must not
      ;; claim to audit code that no longer exists.
      (define inventory-entries (inventory->entries inventory))
      (define stale
        (for*/list ([e (in-list inventory-entries)]
                    #:when (let* ([mod (car e)]
                                  [fpath (build-path q-dir mod)]
                                  [params (if (file-exists? fpath)
                                              (extract-parameters fpath)
                                              '())])
                             (not (member (cadr e) params))))
          e))
      (check-equal?
       stale
       '()
       (if (null? stale)
           "all inventory entries reference live parameters"
           (format
            (string-append
             "Parameter inventory entry for ~a in ~a has no matching make-parameter in production code.~n~n"
             "Remove the stale entry from docs/architecture/parameter-inventory.rktd "
             "(or restore the parameter).~n")
            (if (pair? stale)
                (cadar stale)
                "?")
            (if (pair? stale)
                (caar stale)
                "?")))))

    (test-case "Category distribution is recorded (informational)"
      (define inventory-entries (inventory->entries inventory))
      (define counts (make-hasheq))
      (for ([e (in-list inventory-entries)])
        (hash-set! counts (caddr e) (add1 (hash-ref counts (caddr e) 0))))
      (displayln (format "INFO: parameter inventory distribution: ~a" (hash->list counts)))
      ;; Unification W1 (#9461): +2 — current-model-thinking-idle-timeouts and
      ;; current-model-body-read-timeouts in llm/request-policy.rkt.
      ;; Unification W2 (#9466): +1 — current-request-mechanism-observer in
      ;; llm/stream.rkt (conformance observation seam).
      ;; Unification W3 (#9473): +1 — current-provider-http-sendrecv in
      ;; llm/http-helpers.rkt (injectable HTTP boundary).
      (check-equal? (length inventory-entries)
                    199
                    "parameter inventory should contain 199 audited parameters"))))

;; ============================================================
;; Agent iteration → Runtime boundary tests
;; ============================================================

(define agent-iteration-boundary (cdr (assoc 'agent-iteration-boundary policy)))
(define agent-iter-rule (cdr (assoc 'rule agent-iteration-boundary)))
(define agent-iter-exceptions (cdr (assoc 'exceptions agent-iteration-boundary)))

(define (agent-iter-exception-map)
  ;; basename (string) → exception entry
  (for/hash ([entry (in-list agent-iter-exceptions)])
    (values (symbol->string (car entry)) entry)))

(define agent-iteration-boundary-tests
  (test-suite "agent-iteration-runtime-boundary"

    (test-case "agent-iteration-boundary policy section exists and is loadable"
      (check-true (pair? agent-iteration-boundary)
                  "dependency-policy.rktd must have an agent-iteration-boundary section")
      (check-true (string? agent-iter-rule) "agent-iteration-boundary must declare a rule")
      (check-true (list? agent-iter-exceptions)
                  "agent-iteration-boundary must have an exceptions list")
      (check-pred values
                  (assoc 'exceptions agent-iteration-boundary)
                  "agent-iteration-boundary must have exceptions"))

    (test-case "Agent iteration boundary exceptions have valid metadata"
      (define field-errors
        (for/list ([entry (in-list agent-iter-exceptions)])
          (define name (car entry))
          (define fields (cdr entry))
          (cond
            [(not (string? (exc-field fields 'source))) (format "~a: missing source" name)]
            [(not (list? (exc-field fields 'destinations)))
             (format "~a: missing destinations list" name)]
            [(not (member (exc-field fields 'kind) '(type-only implementation)))
             (format "~a: kind must be type-only or implementation" name)]
            [(not (string? (exc-field fields 'reason))) (format "~a: missing reason" name)]
            [(not (string? (exc-field fields 'owner))) (format "~a: missing owner" name)]
            [(not (string? (exc-field fields 'revisit-by))) (format "~a: missing revisit-by" name)]
            [else #f])))
      (define actual-errors (filter identity field-errors))
      (check-equal? actual-errors
                    '()
                    (format "Invalid agent-iteration-boundary exception metadata: ~a" actual-errors)))

    (test-case "Agent iteration must not import runtime/ except documented exceptions"
      ;; Enforces: agent/iteration/ MUST NOT require runtime/ unless the
      ;; (source, destination) pair is documented in dependency-policy.rktd.
      (define exc-map (agent-iter-exception-map))
      (define violations
        (for*/list ([f (in-list (rkt-files-in-recursive "agent/iteration"))]
                    [req-spec (in-list (extract-requires f))]
                    [path (in-list (require-spec->paths req-spec))]
                    #:when (string-contains? path "runtime/"))
          (define source-name (path->string (file-name-from-path f)))
          (define destination (runtime-path->destination path))
          (define exc (hash-ref exc-map source-name #f))
          (define allowed?
            (and exc (member destination (map symbol->string (exc-field (cdr exc) 'destinations)))))
          (if allowed?
              #f
              (format "~a requires ~a from runtime/" source-name destination))))
      (define actual-violations (filter identity violations))
      (check-equal?
       actual-violations
       '()
       (if (null? actual-violations)
           (format "~a — no new runtime/ imports" agent-iter-rule)
           (format
            (string-append
             "~a~n~nDetected runtime/ imports from agent/iteration/:~n  ~a~n~n"
             "agent/iteration must not acquire Runtime implementation dependencies. "
             "Shared concepts belong in util/iteration/, util/types/, or another neutral layer.~n"
             "If a new exception is genuinely necessary, record it in the agent-iteration-boundary section "
             "of docs/architecture/dependency-policy.rktd with source, destination, reason, owner, and revisit-by.~n")
            agent-iter-rule
            (string-join actual-violations "~n  ")))))

    (test-case "Documented agent-iteration exceptions are not stale"
      ;; Every documented exception must still reference a real runtime import.
      (define stale
        (for/list ([entry (in-list agent-iter-exceptions)])
          (define name (car entry))
          (define fields (cdr entry))
          (define source (exc-field fields 'source))
          (define fpath (build-path q-dir source))
          (if (not (file-exists? fpath))
              (format "~a: source file ~a no longer exists" name source)
              (let ([destinations (map symbol->string (exc-field fields 'destinations))]
                    [reqs (extract-requires fpath)])
                (define imported-destinations
                  (for*/list ([spec (in-list reqs)]
                              [path (in-list (require-spec->paths spec))]
                              #:when (string-contains? path "runtime/"))
                    (runtime-path->destination path)))
                (define still-relevant?
                  (for/or ([d destinations])
                    (member d imported-destinations)))
                (if still-relevant?
                    #f
                    (format
                     "~a: no runtime/ import from ~a matches documented destinations ~a — remove the exception"
                     name
                     source
                     destinations))))))
      (define actual-stale (filter identity stale))
      (check-equal?
       actual-stale
       '()
       (if (null? actual-stale)
           "all documented exceptions reference live runtime/ imports"
           (format
            "Stale agent-iteration boundary exceptions: ~a~n~nRemove exceptions whose imports no longer exist.~n"
            (string-join actual-stale "~n  ")))))

    (test-case "Agent iteration must never import concrete session-config"
      ;; The concrete implementation module is forbidden outright — no
      ;; exception can waive this. Only the neutral opaque type predicate
      ;; (util/types/session-config.rkt) is permitted for Typed Racket.
      (define concrete-forbidden '("runtime/session/session-config.rkt"))
      (define violations
        (for*/list ([f (in-list (rkt-files-in-recursive "agent/iteration"))]
                    [req-spec (in-list (extract-requires f))]
                    [path (in-list (require-spec->paths req-spec))]
                    #:when (member (runtime-path->destination path) concrete-forbidden))
          (path->string (file-name-from-path f))))
      (check-equal?
       violations
       '()
       (if (null? violations)
           "agent/iteration does not import concrete session-config"
           (format
            (string-append
             "agent/iteration/~a imports the concrete session-config implementation "
             "(runtime/session/session-config.rkt).~n~n"
             "Only the neutral opaque type predicate (util/types/session-config.rkt) is allowed for Typed Racket. "
             "Configuration accessors and configuration object pass-through are forbidden.~n")
            (if (pair? violations)
                (car violations)
                "?")))))

    (test-case "Agent iteration type predicates come from neutral util/types/ where required"
      ;; loop-state.rkt (Typed Racket) must use the neutral opaque type shims,
      ;; not the concrete runtime modules, for session-config?/agent-session?/
      ;; working-set?. This verifies the documented neutral-type dependency.
      (define loop-state-path (build-path q-dir "agent" "iteration" "loop-state.rkt"))
      (check-true (file-exists? loop-state-path) "agent/iteration/loop-state.rkt must exist")
      (define reqs (extract-requires loop-state-path))
      (define all-paths (append* (map require-spec->paths reqs)))
      (check-true (for/or ([p all-paths])
                    (string-contains? p "util/types/session-config.rkt"))
                  "loop-state.rkt must import session-config? via util/types/session-config.rkt")
      (check-true (for/or ([p all-paths])
                    (string-contains? p "util/types/session-types.rkt"))
                  "loop-state.rkt must import agent-session? via util/types/session-types.rkt")
      (check-true (for/or ([p all-paths])
                    (string-contains? p "util/types/working-set.rkt"))
                  "loop-state.rkt must import working-set? via util/types/working-set.rkt"))))

;; ============================================================
;; Session-state ownership documentation test
;; ============================================================

(define session-ownership-tests
  (test-suite "session-state-ownership-documentation"

    (test-case "parameter-inventory.rktd documents the session-state ownership rule"
      ;; The inventory header must state that cross-turn session state belongs
      ;; in lifecycle-state, not dynamic parameters. This is a documentation
      ;; invariant, not a semantic proof.
      (define content (file->string inventory-path))
      (check-true
       (regexp-match? #rx"lifecycle-state" content)
       "parameter-inventory.rktd must reference lifecycle-state as the session-state owner")
      (check-true (regexp-match? #rx"cross-turn" content)
                  "parameter-inventory.rktd must document the cross-turn session-state rule"))))

;; ============================================================
;; Run
;; ============================================================

(run-tests parameter-inventory-tests)
(run-tests agent-iteration-boundary-tests)
(run-tests session-ownership-tests)
