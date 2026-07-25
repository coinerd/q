#lang racket/base

;; runtime/task-memory/working-set-continuity.rkt
;; W2 (#8939): Working-set continuity and prompt-boundary classification.
;;
;; Problem (D9 audit): session-lifecycle.rkt unconditionally calls
;; working-set-reset! on every new user message. A related follow-up
;; prompt ("now commit and push") wiped a 30-entry working set to zero.
;;
;; Solution: classify the prompt boundary before deciding whether to reset.
;;   continuation  → retain working set
;;   narrowing     → retain working set
;;   superseding   → retain working set (update objective)
;;   new-task      → archive/reset working set
;;   ambiguous     → retain conservatively (mark tentative)
;;
;; Also provides:
;;   - Snapshot/restore of working-set entries across lifecycle boundaries
;;   - Deduplication of identical adjacent user prompts for provider projection

(require racket/list
         racket/set
         racket/string
         (only-in "../working-set.rkt"
                  working-set?
                  working-set-entries
                  working-set-add!
                  working-set-entry-count
                  working-set-token-count
                  ws-entry?
                  ws-entry-path
                  ws-entry-message-id
                  ws-entry-token-estimate
                  ws-entry-timestamp))

;; ============================================================
;; Prompt-boundary classification
;; ============================================================

;; A boundary-type is one of:
;;   'initial      — no prior objective (first prompt)
;;   'continuation — the new prompt continues the current task
;;   'narrowing    — the new prompt refines/narrows the current task
;;   'superseding  — the new prompt changes the approach but stays in-task
;;   'new-task     — the new prompt starts an explicit new task
;;   'ambiguous    — cannot determine; retain conservatively

(struct prompt-boundary-type (boundary tentative?) #:transparent)

;; Heuristic markers for boundary classification.
;; These are normalized (lowercased, whitespace-collapsed) substring checks.

(define new-task-markers
  '("switch to" "new task"
                "different task"
                "different feature"
                "let's work on something else"
                "start over"
                "move on to"
                "now let's do something different"
                "completely different"))

(define continuation-markers
  '("continue" "proceed"
               "go ahead"
               "next step"
               "now do"
               "then do"
               "after that"
               "keep going"
               "commit and push"
               "push the changes"
               "commit the changes"
               "run the tests"
               "run tests"
               "now commit"
               "now push"
               "now run"
               "step by step"
               "implement the milestone"))

(define narrowing-markers
  '("actually only" "just do"
                    "only do"
                    "wait,"
                    "first just"
                    "instead of"
                    "but only"
                    "narrow it down"
                    "just the"
                    "first write"
                    "let's start with just"))

(define superseding-markers
  '("instead," "rather than"
               "let's use"
               "change the approach"
               "actually, use"
               "switch to using"
               "redo this with"))

;; Normalize text for comparison: trim, collapse whitespace, lowercase.
(define (normalize-text text)
  (define trimmed (string-trim (if (string? text) text "")))
  (define collapsed (regexp-replace* #rx" +" trimmed " "))
  (define collapsed-newlines (regexp-replace* #rx"\n+" collapsed " "))
  (string-downcase collapsed-newlines))

;; Check if normalized text contains any marker.
(define (contains-marker? normalized-text markers)
  (for/or ([m (in-list markers)])
    (string-contains? normalized-text m)))

;; Classify the boundary between a current objective and a new prompt.
;; current-objective: #f, "", or a string describing the active task.
;; new-prompt: the incoming user prompt text.
;; Returns a prompt-boundary-type struct.
(define (classify-prompt-boundary current-objective new-prompt)
  (define norm-prompt (normalize-text new-prompt))
  (cond
    [(or (not current-objective)
         (and (string? current-objective) (string=? (string-trim current-objective) "")))
     (prompt-boundary-type 'initial #f)]
    [(contains-marker? norm-prompt new-task-markers) (prompt-boundary-type 'new-task #f)]
    [(contains-marker? norm-prompt narrowing-markers) (prompt-boundary-type 'narrowing #f)]
    [(contains-marker? norm-prompt superseding-markers) (prompt-boundary-type 'superseding #f)]
    [(contains-marker? norm-prompt continuation-markers) (prompt-boundary-type 'continuation #f)]
    [(has-lexical-overlap? (normalize-text current-objective) norm-prompt)
     (prompt-boundary-type 'continuation #f)]
    [else (prompt-boundary-type 'ambiguous #t)]))

;; Check if two normalized texts share significant word overlap.
(define (has-lexical-overlap? text-a text-b)
  (define words-a (list->set (string-split text-a)))
  (define words-b (list->set (string-split text-b)))
  (define overlap (set-intersect words-a words-b))
  (>= (set-count overlap) 2))

;; Decision: should the working set be reset for this boundary?
;; Only explicit new-task boundaries reset.
(define (should-reset-working-set? boundary)
  (eq? (prompt-boundary-type-boundary boundary) 'new-task))

;; ============================================================
;; Working-set snapshot and restore
;; ============================================================

(define (ws-snapshot-entry? v)
  (and (hash? v)
       (hash-has-key? v 'path)
       (hash-has-key? v 'message-id)
       (hash-has-key? v 'token-estimate)))

(define (ws-snapshot-entry-path snap)
  (hash-ref snap 'path))

(define (ws-snapshot-entry-message-id snap)
  (hash-ref snap 'message-id))

(define (ws-snapshot-entry-token-estimate snap)
  (hash-ref snap 'token-estimate))

(define (ws-snapshot-entry-timestamp snap)
  (hash-ref snap 'timestamp #f))

;; Snapshot all working-set entries to a list of serializable hashes.
(define (working-set-snapshot ws)
  (for/list ([e (in-list (working-set-entries ws))])
    (hash 'path
          (ws-entry-path e)
          'message-id
          (ws-entry-message-id e)
          'token-estimate
          (ws-entry-token-estimate e)
          'timestamp
          (ws-entry-timestamp e))))

;; Restore working-set entries from a snapshot.
;; Optionally filters by available-message-ids (set of IDs still present
;; after compaction/cancellation). Omits unavailable references safely.
(define (restore-from-snapshot! ws snap #:available-message-ids [available #f])
  (for ([entry (in-list snap)]
        #:when (ws-snapshot-entry? entry)
        #:when (or (not available) (set-member? available (hash-ref entry 'message-id))))
    (working-set-add! ws
                      (hash-ref entry 'path)
                      (hash-ref entry 'message-id)
                      (hash-ref entry 'token-estimate)))
  ws)

;; ============================================================
;; Adjacent user-prompt deduplication
;; ============================================================

(define (normalize-prompt-text text)
  (normalize-text text))

;; Deduplicate identical adjacent user prompts, keeping the newest.
(define (dedupe-adjacent-user-prompts/messages messages role-fn content-fn id-fn)
  (define-values (result _last)
    (for/fold ([acc '()]
               [last-user-norm #f])
              ([m (in-list messages)])
      (define role (role-fn m))
      (define content (content-fn m))
      (define norm (normalize-prompt-text content))
      (cond
        [(not (equal? role "user")) (values (cons m acc) #f)]
        ;; Duplicate: replace the last kept user message with the newer one
        [(and last-user-norm (equal? norm last-user-norm)) (values (cons m (cdr acc)) last-user-norm)]
        [else (values (cons m acc) norm)])))
  (reverse result))

;; Generic dedupe for hash-based messages with 'role and 'content keys.
(define (dedupe-adjacent-user-prompts messages)
  (define (role-fn m)
    (if (and (hash? m) (hash-has-key? m 'role))
        (hash-ref m 'role)
        #f))
  (define (content-fn m)
    (if (and (hash? m) (hash-has-key? m 'content))
        (hash-ref m 'content)
        ""))
  (define (id-fn m)
    (if (and (hash? m) (hash-has-key? m 'id))
        (hash-ref m 'id)
        #f))
  (dedupe-adjacent-user-prompts/messages messages role-fn content-fn id-fn))

;; Dedupe with duplicate info: returns (values result dup-info-hash).
(define (dedupe-adjacent-user-prompts/info messages role-fn content-fn id-fn)
  (define-values (result _last dup-hash)
    (for/fold ([acc '()]
               [last-user-norm #f]
               [dh (hash)])
              ([m (in-list messages)])
      (define role (role-fn m))
      (define content (content-fn m))
      (define norm (normalize-prompt-text content))
      (cond
        [(not (equal? role "user")) (values (cons m acc) #f dh)]
        [(and last-user-norm (equal? norm last-user-norm))
         (define mid (id-fn m))
         ;; Duplicate: replace last kept user msg with newer one, count the dup
         (values (cons m (cdr acc))
                 last-user-norm
                 (if mid
                     (hash-update dh mid add1 0)
                     dh))]
        [else (values (cons m acc) norm dh)])))
  (values (reverse result) dup-hash))

;; ============================================================
;; Exports
;; ============================================================

(provide prompt-boundary-type
         prompt-boundary-type?
         prompt-boundary-type-boundary
         prompt-boundary-type-tentative?
         classify-prompt-boundary
         should-reset-working-set?
         has-lexical-overlap?
         normalize-prompt-text
         normalize-text
         ws-snapshot-entry?
         ws-snapshot-entry-path
         ws-snapshot-entry-message-id
         ws-snapshot-entry-token-estimate
         ws-snapshot-entry-timestamp
         working-set-snapshot
         restore-from-snapshot!
         dedupe-adjacent-user-prompts
         dedupe-adjacent-user-prompts/messages
         dedupe-adjacent-user-prompts/info)
