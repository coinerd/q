#lang racket/base

;; tui/state-events/approval-events.rkt -- Approval event handlers
;; STABILITY: internal
;;
;; Extracted from core-handlers.rkt. Handles HITL spawn approval
;; lifecycle events and manages the approval overlay queue.
;; Registration side-effects happen at module load time.

(require racket/string
         racket/match
         racket/list
         (only-in "../../util/event/event.rkt" event event-payload event-ev event-time event?)
         (only-in "../state-types.rkt"
                  ui-state
                  ui-state?
                  ui-state-active-overlay
                  overlay-state
                  overlay-state?
                  overlay-state-type
                  overlay-state-content
                  overlay-state-extra
                  overlay-state-input
                  overlay-state-anchor
                  overlay-state-width
                  overlay-state-height
                  overlay-state-margin)
         (only-in "../render/message-layout.rkt" plain-line)
         (only-in "../approval-channel.rkt" approval-request-view approval-request-pending?)
         "handler-helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Approval helper functions
;; ============================================================

;; Approval requests stay in the overlay's extra field to preserve the ui-state
;; and overlay-state ABIs.  The active request keeps the historical extra keys;
;; queued requests are normalized hashes under 'approval-queue.
(define (approval-request-from-payload payload)
  (and (hash? payload)
       (let* ([request-id (hash-ref payload 'request-id #f)]
              [commitment-digest (hash-ref payload 'commitment-digest #f)]
              [event-presentation-digest (hash-ref payload 'presentation-digest #f)]
              [authoritative-view (and (string? request-id)
                                       (string? commitment-digest)
                                       (string? event-presentation-digest)
                                       (approval-request-view request-id commitment-digest))]
              [authoritative-presentation-digest
               (and authoritative-view (hash-ref authoritative-view 'presentation-digest #f))])
         ;; Event-carried previews are untrusted. Correlation and both digests
         ;; must match before the broker-owned immutable view is displayed.
         (and authoritative-view
              (string? authoritative-presentation-digest)
              (string=? event-presentation-digest authoritative-presentation-digest)
              (let* ([raw-capabilities (hash-ref authoritative-view 'capabilities '())]
                     [capabilities (if (list? raw-capabilities)
                                       raw-capabilities
                                       '())]
                     [raw-preview (hash-ref authoritative-view 'task-preview "")]
                     [task-preview (if (string? raw-preview)
                                       raw-preview
                                       (format "~a" raw-preview))])
                (hasheq 'request-id
                        request-id
                        'commitment-digest
                        commitment-digest
                        'presentation-digest
                        authoritative-presentation-digest
                        'approval-view
                        authoritative-view
                        'capabilities
                        capabilities
                        'task-preview
                        task-preview))))))

(define (approval-request-id request)
  (and (hash? request) (hash-ref request 'request-id #f)))

(define (approval-overlay-queue ov)
  (define extra (overlay-state-extra ov))
  (define queue (and (hash? extra) (hash-ref extra 'approval-queue '())))
  (if (list? queue)
      queue
      '()))

(define (approval-overlay-seen-ids ov)
  (define extra (overlay-state-extra ov))
  (define stored (and (hash? extra) (hash-ref extra 'approval-seen-request-ids #f)))
  (if (list? stored)
      stored
      (filter string?
              (cons (and (hash? extra) (hash-ref extra 'request-id #f))
                    (map approval-request-id (approval-overlay-queue ov))))))

(define (approval-detail-lines view)
  (define (detail label key [default ""])
    (plain-line (format "  ~a: ~a" label (hash-ref view key default))))
  (define common
    (list (detail "Plan" 'plan-kind)
          (detail "Model" 'model-preview)
          (detail "Tools" 'effective-tools '())
          (detail "Working directory" 'cwd-preview)
          (detail "Provider" 'provider-preview)
          (detail "Safe mode" 'safe-mode #f)
          (detail "Max turns" 'max-turns)
          (detail "Parent session" 'parent-session-preview)
          (detail "Parent call" 'parent-call-preview)
          (detail "Tool call" 'tool-call-id)
          (detail "Child" 'child-id)
          (detail "Child session" 'session-id)))
  (define jobs (hash-ref view 'jobs '()))
  (append
   common
   (if (list? jobs)
       (for/list ([job (in-list jobs)])
         (plain-line
          (format "  Job ~a [~a]: model=~a turns=~a caps=~a tools=~a call=~a child=~a session=~a"
                  (hash-ref job 'batch-order "?")
                  (hash-ref job 'job-id "?")
                  (hash-ref job 'model-preview "")
                  (hash-ref job 'max-turns "")
                  (hash-ref job 'effective-capabilities '())
                  (hash-ref job 'effective-tools '())
                  (hash-ref job 'tool-call-id "")
                  (hash-ref job 'child-id "")
                  (hash-ref job 'session-id ""))))
       '())))

(define (approval-request-overlay request queue seen-ids)
  (define capabilities (hash-ref request 'capabilities '()))
  (define task-preview (hash-ref request 'task-preview ""))
  (define caps-str
    (string-join (map (lambda (capability)
                        (if (symbol? capability)
                            (symbol->string capability)
                            (format "~a" capability)))
                      capabilities)
                 ", "))
  (define preview-lines
    (let ([lines (string-split task-preview "
" #:trim? #f)])
      (if (null? lines)
          (list "")
          lines)))
  (define authoritative-view (hash-ref request 'approval-view (hasheq)))
  (define content
    (append (list (plain-line "⚡ Subagent Approval Required")
                  (plain-line (format "  Capabilities: ~a" caps-str)))
            (for/list ([line (in-list preview-lines)]
                       [index (in-naturals)])
              (plain-line (format "  ~a: ~a" (if (zero? index) "Task" "    ") line)))
            (approval-detail-lines authoritative-view)
            (list (plain-line "") (plain-line "  [y] Approve   [n] Deny   [Esc] Cancel"))))
  (overlay-state 'approval-prompt
                 content
                 ""
                 'top-left
                 #f
                 #f
                 0
                 (hasheq 'capabilities
                         capabilities
                         'task-preview
                         task-preview
                         'request-id
                         (hash-ref request 'request-id)
                         'commitment-digest
                         (hash-ref request 'commitment-digest)
                         'presentation-digest
                         (hash-ref request 'presentation-digest "")
                         'approval-view
                         authoritative-view
                         'approval-queue
                         queue
                         'approval-seen-request-ids
                         seen-ids)))

;; Remove exactly one correlated request.  If it is active, promote the next
;; queued request in FIFO order; if it is queued, leave the active request in
;; place.  This helper is shared with the key handler so a failed/stale channel
;; delivery can never dismiss a newer prompt.
(define (approval-overlay-remove-request state request-id)
  (define ov (ui-state-active-overlay state))
  (cond
    [(not (and (string? request-id) ov (eq? (overlay-state-type ov) 'approval-prompt))) state]
    [else
     (define extra (overlay-state-extra ov))
     (define active-id (and (hash? extra) (hash-ref extra 'request-id #f)))
     (define queue (approval-overlay-queue ov))
     (define seen-ids (approval-overlay-seen-ids ov))
     (cond
       [(equal? request-id active-id)
        (if (null? queue)
            (struct-copy ui-state state [active-overlay #f])
            (struct-copy ui-state
                         state
                         [active-overlay
                          (approval-request-overlay (car queue) (cdr queue) seen-ids)]))]
       [(ormap (lambda (request) (equal? request-id (approval-request-id request))) queue)
        (define remaining
          (filter (lambda (request) (not (equal? request-id (approval-request-id request)))) queue))
        (define new-extra (hash-set extra 'approval-queue remaining))
        (struct-copy ui-state
                     state
                     [active-overlay (struct-copy overlay-state ov [extra new-extra])])]
       [else state])]))

;; Handle a correlated spawn approval request.  Existing approval prompts are
;; never replaced: new requests queue FIFO and duplicate deliveries are ignored.
(define (handle-spawn-approval-requested state evt)
  (define request (approval-request-from-payload (event-payload evt)))
  (cond
    [(not request) state]
    [else
     (define request-id (approval-request-id request))
     (define ov (ui-state-active-overlay state))
     (cond
       [(and ov (eq? (overlay-state-type ov) 'approval-prompt))
        (define seen-ids (approval-overlay-seen-ids ov))
        (if (member request-id seen-ids)
            state
            (let* ([queue (approval-overlay-queue ov)]
                   [old-extra (overlay-state-extra ov)]
                   [base-extra (if (hash? old-extra)
                                   old-extra
                                   (hasheq))]
                   [new-extra
                    (hash-set (hash-set base-extra 'approval-queue (append queue (list request)))
                              'approval-seen-request-ids
                              (append seen-ids (list request-id)))])
              (struct-copy ui-state
                           state
                           [active-overlay (struct-copy overlay-state ov [extra new-extra])])))]
       [else
        (struct-copy ui-state
                     state
                     [active-overlay (approval-request-overlay request '() (list request-id))])])]))

(define (approval-overlay-request-digest state request-id)
  (define overlay (ui-state-active-overlay state))
  (and overlay
       (eq? (overlay-state-type overlay) 'approval-prompt)
       (let* ([extra (overlay-state-extra overlay)]
              [active-id (and (hash? extra) (hash-ref extra 'request-id #f))]
              [active-digest (and (hash? extra) (hash-ref extra 'commitment-digest #f))])
         (if (equal? request-id active-id)
             active-digest
             (for/or ([request (in-list (approval-overlay-queue overlay))])
               (and (equal? request-id (approval-request-id request))
                    (hash-ref request 'commitment-digest #f)))))))

(define (handle-spawn-approval-terminal state evt)
  (define payload (event-payload evt))
  (define request-id (and (hash? payload) (hash-ref payload 'request-id #f)))
  (define commitment-digest (and (hash? payload) (hash-ref payload 'commitment-digest #f)))
  (define displayed-digest
    (and (string? request-id) (approval-overlay-request-digest state request-id)))
  (if (and (string? commitment-digest)
           (equal? commitment-digest displayed-digest)
           (not (approval-request-pending? request-id commitment-digest)))
      (approval-overlay-remove-request state request-id)
      state))

;; ============================================================
;; Exports
;; ============================================================

(provide handle-spawn-approval-requested
         handle-spawn-approval-terminal
         approval-overlay-remove-request)

;; ============================================================
;; Register handlers at module load time
;; ============================================================

(register-event-reducer! "mas.spawn-approval-requested" handle-spawn-approval-requested)
(register-event-reducer! "mas.spawn-approval-terminal" handle-spawn-approval-terminal)
