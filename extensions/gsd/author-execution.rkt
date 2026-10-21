#lang racket/base
;; Ordinary production executor phases. The owner, not the model, commits and
;; runs checks. Evidence capture does not confer review or delivery authority.
(require racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "../../util/worker-tool-policy.rkt" current-worker-tool-policy)
         "supplemental-author-contract.rkt"
         (only-in "../../agent/state.rkt" current-empty-response-nudge)
         "red-checkpoint.rkt"
         "delivery-receipt.rkt"
         "delivery-journal.rkt"
         (only-in "evidence-author-journal.rkt"
                  start-evidence-author-repair!
                  complete-evidence-author-no-edit!)
         "wave-runner-port.rkt"
         (only-in "policy.rkt" current-gsd-wave-failure-context)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide run-proactive-author-phases!
         run-retained-author-phases!
         run-supplemental-test-author!)
(define (need value message)
  (unless value
    (error 'author-execution "~a" message))
  value)
(define (git repo . args)
  (define out (open-output-string))
  (need (parameterize ([current-directory repo]
                       [current-output-port out]
                       [current-error-port (open-output-nowhere)])
          (apply system* (find-executable-path "git") args))
        (format "owner Git operation failed: ~s" args))
  (string-trim (get-output-string out)))
(define (commit-phase! repo before files tests phase attempt guard #:allow-empty? [allow-empty? #f])
  (need (guard) "lost authority before phase commit")
  (need (equal? before (git repo "rev-parse" "HEAD")) "executor changed the owner-controlled commit")
  (define changed
    (remove-duplicates
     (append (string-split (git repo "diff" "--name-only" "--no-renames" "HEAD" "--") "\n")
             (string-split (git repo "ls-files" "--others" "--exclude-standard") "\n"))))
  (need (or allow-empty? (pair? changed)) "phase made no progress")
  (for ([path (in-list changed)])
    (need (and (member path files)
               (if (eq? phase 'test-author)
                   (member path tests)
                   (not (member path tests))))
          (format "~a changed forbidden frozen path ~s" phase path)))
  (when (pair? changed)
    (apply git repo (append '("add" "--") changed))
    (need (guard) "lost authority before owner commit")
    (git repo "commit" "-qm" (format "GSD ~a ~a" phase attempt)))
  (need (committed-delivery-snapshot repo) "phase left an uncommitted checkout"))
(define (phase-instructions phase)
  (case phase
    [(test-author)
     (string-append
      "\nCURRENT OWNED PHASE: TEST AUTHOR ONLY. This is not a review rejection.\n"
      "Write the regression in the frozen test FILES only; do not implement the fix.\n"
      "Use an exported literal RackUnit evidence-suite with the ordinary module+ test running that same suite.\n"
      "It must fail behaviorally on the current source. No fabricated output, exits, skipped checks or weakened expectations.\n"
      "Bash and custom mutation tools are unavailable. The owner commits and runs the frozen focused Verify.\n"
      "Return when test authoring is complete. Implementation will be a separate dispatch after authenticated RED.\n")]
    [else
     (string-append
      "\nCURRENT OWNED PHASE: IMPLEMENTATION AFTER AUTHENTICATED RED.\n"
      "Implement only the remaining frozen non-test FILES. Do not alter the frozen tests, runner or declaration.\n"
      "Bash and custom mutation tools are unavailable. The owner commits, checks identical tests, and runs full frozen Verify.\n"
      "Do not claim review approval, delivery, canary or F11; those remain independent gates.\n")]))
(define (dispatch-phase! repo
                         files
                         tests
                         phase
                         attempt
                         run
                         guard
                         after-phase
                         #:instructions [instructions (phase-instructions phase)]
                         #:allow-empty? [allow-empty? #f])
  (need (guard) "stale authority before model dispatch")
  (define before (git repo "rev-parse" "HEAD"))
  (define result
    (parameterize ([current-gsd-author-phase
                    (hasheq 'phase phase 'repo repo 'files files 'tests tests)]
                   [current-gsd-wave-failure-context
                    (string-append (or (current-gsd-wave-failure-context) "") instructions)])
      (coerce-run-result (run))))
  (after-phase)
  (when (eq? (wave-execution-outcome-kind result) 'done)
    (commit-phase! repo before files tests phase attempt guard #:allow-empty? allow-empty?))
  result)
(define (run-retained-author-phases! base
                                     plan
                                     wave
                                     attempt
                                     fence
                                     run
                                     #:guard guard
                                     #:after-phase [after-phase void])
  (with-handlers ([exn:fail? (lambda (e)
                               (wave-execution-outcome
                                'failed
                                (string-append "Evidence author repair blocked: " (exn-message e))))])
    (define journal (load-delivery-journal base plan wave))
    (define marker (journal-evidence-author-repair journal))
    (need (and (evidence-author-repair-pending? journal) (equal? (hash-ref marker 'state) "pending"))
          "author allowance already launched or unavailable; ambiguous dispatch is not repeated")
    (define repo (simplify-path (path->complete-path (current-directory))))
    (define context (hash-ref marker 'context))
    (need (equal? (path->directory-path repo)
                  (simplify-path (path->directory-path (hash-ref context 'repo))))
          "author source checkout differs from authenticated execution CWD")
    (define source (committed-delivery-snapshot repo))
    (define old (hash-ref marker 'old-receipt))
    (need (and source
               (for/and ([key '(repo branch origin head tree)])
                 (equal? (hash-ref source key) (hash-ref old key))))
          "retained author source drifted from the exact receipt")
    (define files (frozen-author-files base plan wave))
    (need (equal? files (hash-ref marker 'frozen-files)) "author frozen scope changed")
    (define tests (filter (lambda (p) (string-prefix? p "tests/")) files))
    (need (pair? tests) "frozen scope has no test-author FILES")
    ;; Durable at-most-once launch intent. Crash/lost response after this
    ;; write pauses instead of buying another model attempt or fake rejection.
    (start-evidence-author-repair! base plan wave attempt fence #:guard guard)
    (define authored
      ;; BUG-0084: an EMPTY test-author phase is a legitimate verdict here —
      ;; this durable permission exists precisely when the prior failure was
      ;; lane-side (no rejection findings demand new evidence). Refusing an
      ;; empty phase would force a manufactured edit. The final tree check
      ;; below still distinguishes the honest no-edit terminal from real work.
      (dispatch-phase!
       repo
       files
       tests
       'test-author
       attempt
       run
       guard
       after-phase
       #:allow-empty? #t
       #:instructions
       (string-append
        "\nEVIDENCE AUTHOR REPAIR (separate default-one allowance, NOT REQUEST_CHANGES).\n"
        "Retain existing implementation. If new evidence is genuinely needed, edit only frozen test FILES into a\n"
        "compatible exported RackUnit evidence-suite.\n"
        "If the prior failure was lane-side (review/verification infrastructure) and the retained evidence is\n"
        "complete, return WITHOUT edits; the owner routes the wave to a fresh review.\n"
        "When authoring, the ordinary module+ test must run that exact suite. Add a meaningful behavioral regression, not a vacuous check.\n"
        "This is POSTHOC reproduction, not historical RED-first. Tests should pass on the current candidate.\n"
        "A trusted baseline-with-test-overlay probe must later demonstrate genuine assertion failure on baseline source.\n"
        "No shell, Git, fabricated output, approval, F11 or canary authority is granted. Return after test authoring.\n")))
    (cond
      [(not (eq? (wave-execution-outcome-kind authored) 'done)) authored]
      [else
       (define pinned-tests (apply git repo (append '("ls-tree" "HEAD" "--") tests)))
       (define implemented
         (dispatch-phase!
          repo
          files
          tests
          'implementation
          attempt
          run
          guard
          after-phase
          #:allow-empty? #t
          #:instructions
          (string-append
           "\nEVIDENCE AUTHOR REPAIR: preserve all authored tests byte-for-byte.\n"
           "If implementation needs correction, edit only remaining frozen non-test FILES. Otherwise return without edits.\n"
           "No shell or custom mutations. The owner will run full Verify, then independent evidence probing and review.\n")))
       (need (equal? pinned-tests (apply git repo (append '("ls-tree" "HEAD" "--") tests)))
             "implementation changed the frozen authored tests")
       ;; BUG-0084: distinguish the honest no-edit terminal from real work.
       ;; When BOTH phases returned without a single change, the retained
       ;; implementation IS the verdict; stamp the durable "retained" marker
       ;; so the coordinator routes this wave to a fresh review instead of
       ;; stranding it in an unresolvable "running" checkpoint.
       (define snapshot (committed-delivery-snapshot repo))
       (if (and snapshot
                (equal? (hash-ref snapshot 'tree) (hash-ref (hash-ref marker 'old-receipt) 'tree)))
           (begin
             (complete-evidence-author-no-edit! base
                                                plan
                                                wave
                                                (hash-ref snapshot 'tree)
                                                #:guard guard)
             (wave-execution-outcome
              'done
              "Evidence author returned without edits; implementation retained; fresh review is next"))
           implemented)])))
;; The descriptor is already authenticated by the owned controller. Keep all
;; projections dynamically bound until the owned runner returns, including
;; same-invocation continuations. The runtime captures this generic policy.
(define (run-supplemental-test-author! repo contract attempt run guard after-phase)
  (define scope (supplemental-author-contract-scope contract))
  (need (equal? (path->directory-path (simplify-path (path->complete-path repo) #f))
                (path->directory-path (hash-ref scope 'repo)))
        "supplemental contract checkout mismatch")
  (define before (git repo "rev-parse" "HEAD"))
  (need (guard) "stale authority before model dispatch")
  (define result
    (parameterize ([current-supplemental-author-contract contract]
                   [current-gsd-author-phase scope]
                   [current-worker-tool-policy (make-supplemental-worker-tool-policy contract)]
                   [current-gsd-wave-failure-context #f]
                   [current-empty-response-nudge (render-supplemental-author-prompt contract)])
      (coerce-run-result (run))))
  (after-phase)
  (when (eq? (wave-execution-outcome-kind result) 'done)
    (commit-phase! repo
                   before
                   (hash-ref scope 'files)
                   (hash-ref scope 'tests)
                   'test-author
                   attempt
                   guard))
  result)

(define (run-proactive-author-phases! base
                                      plan
                                      wave
                                      attempt
                                      fence
                                      run
                                      #:guard guard
                                      #:after-phase [after-phase void])
  (with-handlers ([exn:fail? (lambda (e)
                               (wave-execution-outcome 'failed
                                                       (string-append "Owned author phase blocked: "
                                                                      (exn-message e))))])
    (define repo (simplify-path (path->complete-path (current-directory))))
    (define files (frozen-author-files base plan wave))
    (define tests (filter (lambda (path) (string-prefix? path "tests/")) files))
    (define (dispatch phase)
      (dispatch-phase! repo files tests phase attempt run guard after-phase))
    (define start (begin-red-test-author! base plan wave repo attempt fence #:guard guard))
    (define authored
      (if (equal? (hash-ref start 'status) "red-ready")
          (wave-execution-outcome 'done "Authenticated checkpoint retained")
          (dispatch 'test-author)))
    (cond
      [(not (eq? (wave-execution-outcome-kind authored) 'done)) authored]
      [else
       (define checkpoint
         (if (equal? (hash-ref start 'status) "red-ready")
             start
             (capture-red-checkpoint! base
                                      plan
                                      wave
                                      repo
                                      (hash-ref (hash-ref start 'baseline-source) 'head)
                                      attempt
                                      fence
                                      #:guard guard)))
       (need (equal? (hash-ref checkpoint 'status) "red-ready")
             (format "preimplementation RED refused: ~s" checkpoint))
       (validate-red-checkpoint! base plan wave repo attempt fence #:guard guard)
       (begin-red-implementation! base plan wave repo attempt fence #:guard guard)
       (define implemented (dispatch 'implementation))
       (when (eq? (wave-execution-outcome-kind implemented) 'done)
         (validate-red-checkpoint! base plan wave repo attempt fence #:guard guard)
         (verify-red-checkpoint-green! base plan wave repo attempt fence #:guard guard))
       ;; Only ordinary full Verify may turn this terminal outcome into a
       ;; receipt. No checkpoint or candidate-only run is a GREEN receipt.
       implemented])))
