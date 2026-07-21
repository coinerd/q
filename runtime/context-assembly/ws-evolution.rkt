#lang racket/base

;; runtime/context-assembly/ws-evolution.rkt — Working-set evolution on state transitions
;; v0.75.4 W0: Evolves working-set when task-state changes
;;
;; When the agent transitions from exploration to planning/implementation,
;; raw file contents in the working-set are replaced with conclusions,
;; reducing token count while preserving key information.

(require (only-in "../working-set.rkt"
                  working-set?
                  working-set-entries
                  working-set-reset!
                  working-set-entry-count
                  working-set-token-count
                  working-set-selective-remove!
                  ws-entry?
                  ws-entry-path)
         (only-in "task-state.rkt"
                  task-state?
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "task-conclusion.rkt" task-conclusion? task-conclusion-text)
         (only-in "../../util/fsm/fsm.rkt" fsm-state? fsm-state-name)
         racket/set
         racket/string
         racket/path)

(provide evolve-working-set-for-state
         working-set-selective-keep
         evolution-result
         evolution-result?
         evolution-result-kept-entries
         evolution-result-archived-entries
         evolution-result-evicted-conclusions
         evolve-working-set-for-state/result
         ws-entry-matches-tags?)

;; v0.99.54 W4 R-5: Tag-based path classification for working-set evolution.
;; Uses basename-derived tags instead of raw substring matching to avoid
;; false positives (e.g. "testimonies.txt", "errors.log", "specs.md").
;; Returns one of 'test, 'spec, 'error, 'validation, or #f.
(define (first-path-component p)
  ;; Extract the first directory component from a path string.
  ;; e.g. "tests/test-foo.rkt" -> "tests"
  ;; Returns #f for bare filenames like "error.log".
  (define idx (regexp-match-positions #px"/" p))
  (and idx (substring p 0 (car (car idx)))))

(define (path->tag p)
  (and (string? p)
       (let* ([bn (path->string (file-name-from-path (string->path p)))]
              [bn-lower (string-downcase bn)]
              [bn-stem (let ([dot (regexp-match-positions #px"[.]" bn-lower)])
                         (if dot
                             (substring bn-lower 0 (caar dot))
                             bn-lower))]
              [dir (first-path-component p)])
         (cond
           ;; Test files: in "tests/" or "test/" dir, filename contains "test" component
           [(or (and dir (member dir '("tests" "test")))
                (string-prefix? bn-stem "test-")
                (string-suffix? bn-stem "-test")
                (string-contains? bn-stem "_test"))
            'test]
           ;; Spec files: in "spec/" dir, filename starts with "spec" or contains "_spec"
           [(or (equal? dir "spec")
                (string-prefix? bn-stem "spec")
                (string-suffix? bn-stem "-spec")
                (string-contains? bn-stem "_spec"))
            'spec]
           ;; Error files: in "error/" or "errors/" dir, or stem contains "error"
           [(or (and dir (member dir '("error" "errors")))
                (string-suffix? bn-stem "-errors")
                (string-contains? bn-stem "-error-")
                (string-contains? bn-stem "_error")
                (string-contains? bn-stem "error"))
            'error]
           ;; Validation files: in "validation/" or "validate/" dir, or stem is "validation"
           [(or (equal? dir "validation") (equal? dir "validate") (equal? bn-stem "validation"))
            'validation]
           [else #f]))))

(define (ws-entry-matches-tags? entry tag)
  (eq? (path->tag (ws-entry-path entry)) tag))

;; working-set-selective-keep : working-set? (-> any/c boolean?) → void?
;; Keep only entries matching predicate. Removes all others.
(define (working-set-selective-keep ws keep-pred?)
  (working-set-selective-remove! ws keep-pred?))

;; evolve-working-set-for-state : working-set? task-state? task-state? (listof task-conclusion?) → (listof task-conclusion?)
;;
;; Evolves the working-set based on state transition.
;; Returns the conclusions that should be injected.
;; old-state: the previous task state (or #f if unknown)
;; new-state: the new task state
;; conclusions: current conclusions list
;; Coerce fsm-state? struct or raw symbol to a state-name symbol.
;; The runtime stores task states as raw symbols (e.g., 'exploration)
;; in agent-session-task-fsm-state, while FSM singletons are fsm-state? structs.
;; Both must be accepted.
(define (state->name s)
  (cond
    [(not s) #f]
    [(fsm-state? s) (fsm-state-name s)]
    [(symbol? s) s]
    [else #f]))

(define (evolve-working-set-for-state ws old-state new-state conclusions)
  (define old-name (state->name old-state))
  (define new-name (state->name new-state))

  (cond
    ;; exploration → planning or implementation: clear ws, return conclusions
    [(and (eq? old-name 'exploration) (or (eq? new-name 'planning) (eq? new-name 'implementation)))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; v0.99.54 W4 R-5: Use path->tag for file classification instead of raw substring matching
    ;; implementation → debugging: keep error-related files
    [(and (eq? old-name 'implementation) (eq? new-name 'debugging))
     (define tags-to-keep '(test error spec))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]

    ;; debugging → implementation: clear selective entries
    [(and (eq? old-name 'debugging) (eq? new-name 'implementation))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; GAP-B v0.97.8: planning → implementation: clear ws, return conclusions
    [(and (eq? old-name 'planning) (eq? new-name 'implementation))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; GAP-B v0.97.8: planning → verification: keep spec/test files, return conclusions
    [(and (eq? old-name 'planning) (eq? new-name 'verification))
     (define tags-to-keep '(test spec validation))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]

    ;; GAP-B v0.97.8: planning → debugging: keep error/test files, return conclusions
    [(and (eq? old-name 'planning) (eq? new-name 'debugging))
     (define tags-to-keep '(test error spec))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]

    ;; GAP-B v0.97.8: verification → implementation: clear ws, return conclusions
    ;; GAP-C v0.97.10: implementation → verification: keep test/spec, return conclusions
    [(and (eq? old-name 'implementation) (eq? new-name 'verification))
     (define tags-to-keep '(test spec validation))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]

    ;; GAP-C v0.97.10: verification → debugging: keep error/test files
    [(and (eq? old-name 'verification) (eq? new-name 'debugging))
     (define tags-to-keep '(test error spec))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]

    ;; GAP-C v0.97.10: debugging → verification: keep test/spec/validation files
    [(and (eq? old-name 'debugging) (eq? new-name 'verification))
     (define tags-to-keep '(test spec validation))
     (working-set-selective-remove! ws
                                    (lambda (e)
                                      (and (string? (ws-entry-path e))
                                           (memq (path->tag (ws-entry-path e)) tags-to-keep))))
     (filter task-conclusion? conclusions)]
    [(and (eq? old-name 'verification) (eq? new-name 'implementation))
     (working-set-reset! ws)
     (filter task-conclusion? conclusions)]

    ;; any → idle: full reset
    [(eq? new-name 'idle)
     (working-set-reset! ws)
     '()]

    ;; No evolution needed for other transitions
    [else (filter task-conclusion? conclusions)]))

;; ════════════════════════════════════════════════════════════════
;; v0.77.1 W1.1: Structured evolution result
;; ════════════════════════════════════════════════════════════════

(struct evolution-result
        (kept-entries ; (listof ws-entry?) — entries remaining in the WS
         archived-entries ; (listof ws-entry?) — entries removed (for append-only log)
         evicted-conclusions) ; (listof task-conclusion?) — conclusions to inject
  #:transparent)

;; evolve-working-set-for-state/result :
;;   working-set? task-state? task-state? (listof task-conclusion?) -> evolution-result?
;;
;; Like evolve-working-set-for-state but returns a structured result with
;; archived entries for append-only persistence.
(define (evolve-working-set-for-state/result ws old-state new-state conclusions)
  ;; GAP-5: Skip evolution when old-state equals new-state
  (define old-name (state->name old-state))
  (define new-name (state->name new-state))
  (if (equal? old-name new-name)
      #f ;; No evolution needed — same state
      (let ()
        (define entries-before (working-set-entries ws))
        (define injected (evolve-working-set-for-state ws old-state new-state conclusions))
        (define entries-after (working-set-entries ws))
        ;; Compute archived: entries present before but not after
        (define after-paths
          (for/set ([e (in-list entries-after)])
            (ws-entry-path e)))
        (define archived
          (for/list ([e (in-list entries-before)]
                     #:when (not (set-member? after-paths (ws-entry-path e))))
            e))
        (evolution-result entries-after archived injected))))
