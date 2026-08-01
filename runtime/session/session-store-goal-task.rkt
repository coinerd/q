#lang racket/base
;; STABILITY: evolving

;; runtime/session/session-store-goal-task.rkt — Goal state & archive marker persistence
;;
;; Extracted from session-store.rkt (v0.99.58 W2-2 P1-ST).
;; Provides:
;;   Goal state persistence: append-goal-state!, load-latest-goal-state
;;   Archive markers: append-archive-marker!, load-conclusions-archived
;;   Shared helper: conclusion-hash->json-safe (also used by session-store.rkt)
;;
;; Cycle-breaking pattern (same as session-store-tree.rkt v0.74.1 W0):
;; This module does NOT require session-store.rkt. Instead, session-store.rkt
;; injects append-entry! and load-session-log via runtime parameters at load time.

(require racket/contract
         racket/hash
         json
         (only-in "../goal/goal-state.rkt"
                  goal-state
                  goal-state?
                  goal-state-id
                  goal-state-updated-at
                  goal-state->hash
                  hash->goal-state
                  evaluation-result
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason
                  evaluation-result-check-results
                  evaluation-result-model-used
                  evaluation-result-token-cost
                  evaluation-result->hash
                  hash->evaluation-result)
         (only-in "../context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  conclusion->hash
                  hash->conclusion)
         (only-in "../goal/goal-evidence.rkt"
                  evidence-provenance
                  evidence-provenance?
                  evidence-provenance->hash
                  hash->evidence-provenance)
         (only-in "../../util/content/content-parts.rkt" make-text-part text-part-text)
         (only-in "../../util/message/message.rkt"
                  make-message
                  message
                  message-content
                  message-kind
                  message?)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/error/error-helpers.rkt" with-safe-fallback))

;; ── Runtime parameters — set by session-store.rkt on load ──
;; Breaks potential circular dependency: this module needs append-entry! and
;; load-session-log, which are defined in session-store.rkt. Using parameters
;; avoids the cycle without code duplication.

(define current-append-entry!-gt (make-parameter #f))
(define current-load-session-log-gt (make-parameter #f))

(provide current-append-entry!-gt
         current-load-session-log-gt

         (contract-out
          [append-goal-state! (path-string? goal-state? . -> . void?)]
          [append-goal-state-snapshot! (path-string? goal-state? . -> . void?)]
          [load-latest-goal-state (path-string? . -> . (or/c goal-state? #f))]
          [append-evaluation-result!
           (path-string? evaluation-result? exact-nonnegative-integer? . -> . void?)]
          [load-goal-evaluations
           (path-string? . -> . (listof (cons/c exact-nonnegative-integer? evaluation-result?)))]
          [append-evidence-result! (path-string? evidence-provenance? . -> . void?)]
          [load-goal-evidence (path-string? . -> . (listof evidence-provenance?))]
          [append-archive-marker! (path-string? task-conclusion? . -> . void?)]
          [load-conclusions-archived (path-string? . -> . (listof task-conclusion?))]
          [conclusion-hash->json-safe (-> hash? hash?)]))

;; ============================================================
;; Shared helper: JSON-safe conclusion hash conversion
;; ============================================================
;; Convert conclusion hash to JSON-safe (symbols → strings).
;; Used by append-archive-marker! here, and by make-task-conclusion-message
;; in session-store.rkt (via re-export).

(define (conclusion-hash->json-safe h)
  (for/hash ([(k v) (in-hash h)])
    (values k
            (cond
              [(symbol? v) (symbol->string v)]
              [(list? v)
               (map (lambda (x)
                      (if (symbol? x)
                          (symbol->string x)
                          x))
                    v)]
              [else v]))))

;; ============================================================
;; Goal state persistence (v0.71.0)
;; ============================================================

(define (append-goal-state! path gs)
  ;; Store goal-state as a JSON string in a system message
  ;; so it survives message->jsexpr serialization
  (define gs-json-str (jsexpr->string (goal-state->hash gs)))
  ((current-append-entry!-gt) path
                              (make-message (goal-state-id gs)
                                            #f
                                            'system
                                            'goal-state
                                            (list (make-text-part gs-json-str))
                                            (goal-state-updated-at gs)
                                            #f)))

;; ============================================================
;; Goal state snapshot persistence (v0.99.78 W2, G-9)
;; ============================================================
;; On every goal-state mutation the runner writes a JSON-safe snapshot as a
;; structured `goal.state` entry (NEW kind — Decision D2). The snapshot
;; carries goal-text, status, turns-used, max-turns, updated-at and the full
;; goal-state hash, so an operator can grep the session log for "what goal is
;; running and where it stands" at any time.

(define (append-goal-state-snapshot! path gs)
  (define payload (goal-state->hash gs))
  ((current-append-entry!-gt) path
                              (make-message (goal-state-id gs)
                                            #f
                                            'system
                                            'goal.state
                                            (list (make-text-part (jsexpr->string payload)))
                                            (goal-state-updated-at gs)
                                            (hasheq))))

(define (load-latest-goal-state path)
  (define entries ((current-load-session-log-gt) path))
  ;; Prefer the v0.99.78 W2 structured `goal.state` snapshots; fall back to
  ;; legacy `goal-state` entries so pre-W2 logs remain readable.
  (define snapshot-entries
    (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal.state))) entries))
  (define goal-entries
    (if (pair? snapshot-entries)
        snapshot-entries
        (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal-state))) entries)))
  (if (null? goal-entries)
      #f
      (let* ([last-entry (car (reverse goal-entries))]
             [content (message-content last-entry)]
             [json-str (if (string? content)
                           content
                           (text-part-text (car content)))])
        (hash->goal-state (string->jsexpr json-str)))))

;; ============================================================
;; Evaluation decision persistence (v0.99.78 W1, G-8)
;; ============================================================
;; Evaluator decisions are appended to the session log as structured
;; `goal.evaluation` entries (NEW kind — no schema change to existing
;; session-log kinds, Decision D2). The persisted payload is identical
;; to the `goal.evaluated` event payload so the UI and the log cannot
;; diverge (event parity). An auditor can reconstruct every evaluator
;; decision from the session log without the live process.

(define (append-evaluation-result! path er turn)
  (define payload (hasheq 'turn turn 'evaluation (evaluation-result->hash er)))
  ((current-append-entry!-gt) path
                              (make-message (generate-id)
                                            #f
                                            'system
                                            'goal.evaluation
                                            (list (make-text-part (jsexpr->string payload)))
                                            (current-seconds)
                                            (hasheq))))

(define (load-goal-evaluations path)
  (define entries ((current-load-session-log-gt) path))
  (define eval-entries
    (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal.evaluation))) entries))
  (for/list ([e (in-list eval-entries)])
    (define content (message-content e))
    (define text
      (if (string? content)
          content
          (text-part-text (car content))))
    (define raw (string->jsexpr text))
    (cons (hash-ref raw 'turn 0) (hash->evaluation-result (hash-ref raw 'evaluation)))))

;; ============================================================
;; Verification evidence provenance persistence (v0.99.78 W3, G-5)
;; ============================================================
;; Verification results are appended to the session log as structured
;; `goal.evidence` entries (NEW kind — no schema change to existing
;; session-log kinds, Decision D2). Each entry carries the provenance
;; record (evidence-id, kind, base-sha, tree-hash, captured-at, result),
;; so no verification result can be accepted after its base/tree changed.

(define (append-evidence-result! path prov)
  ((current-append-entry!-gt)
   path
   (make-message (generate-id)
                 #f
                 'system
                 'goal.evidence
                 (list (make-text-part (jsexpr->string (evidence-provenance->hash prov))))
                 (current-seconds)
                 (hasheq))))

(define (load-goal-evidence path)
  (define entries ((current-load-session-log-gt) path))
  (define ev-entries
    (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal.evidence))) entries))
  (for/list ([e (in-list ev-entries)])
    (define content (message-content e))
    (define text
      (if (string? content)
          content
          (text-part-text (car content))))
    (hash->evidence-provenance (string->jsexpr text))))

;; ============================================================
;; Archive markers (v0.77.1 W1.4)
;; ============================================================

;; v0.77.1 W1.4: Append-only archive markers for evicted conclusions.
;; Archived conclusions are logged but skipped by load-conclusions by default.
;; Use load-conclusions-archived to include them.

(define (append-archive-marker! path conclusion)
  (define json-safe (conclusion-hash->json-safe (conclusion->hash conclusion)))
  ((current-append-entry!-gt) path
                              (make-message (generate-id)
                                            #f
                                            'system
                                            'task-conclusion-archived
                                            (list (make-text-part (jsexpr->string json-safe)))
                                            (current-seconds)
                                            (hasheq))))

(define (load-conclusions-archived path)
  (define entries ((current-load-session-log-gt) path))
  (define conc-entries
    (filter (lambda (e)
              (and (message? e)
                   (or (eq? (message-kind e) 'task-conclusion)
                       (eq? (message-kind e) 'task-conclusion-archived))))
            entries))
  (for/list ([e (in-list conc-entries)]
             #:when (message? e))
    (define content (message-content e))
    (define text
      (if (string? content)
          content
          (text-part-text (car content))))
    (with-safe-fallback
     #f
     (define raw (string->jsexpr text))
     (define restored
       (for/hash ([(k v) (in-hash raw)])
         (values k
                 (cond
                   [(and (string? v) (memq k '(category fsm-state-origin))) (string->symbol v)]
                   [(and (list? v) (eq? k 'relevance-tags))
                    (map (lambda (x)
                           (if (string? x)
                               (string->symbol x)
                               x))
                         v)]
                   [(and (list? v) (eq? k 'origin-message-ids)) v]
                   [else v]))))
     (hash->conclusion restored))))
