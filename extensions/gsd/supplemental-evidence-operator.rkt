#lang racket/base
;; STABILITY: internal
;; BUG-0088 C1: trusted operator presentation over the EXISTING interactive
;; approval broker. This module is the ONLY bridge between retained private
;; proposals and a real operator decision:
;;   - the exact proposal sha256 is the broker commitment digest;
;;   - the full immutable retained presentation is registered with it;
;;   - the approval-request/terminal events ride the owner UI publisher
;;     (tools/permission-gate.rkt pattern — the publisher only transports
;;     correlation telemetry and can never grant);
;;   - the ONLY outcome is the broker's real opaque one-use grant (or #f).
;; No TTY/stdin/prompt/boolean substitute, no policy grant, no grant
;; constructor, no model tool and no /replan change exists here. A batch or
;; CLI context without an ACTIVE interactive channel refuses honestly.
;; Consumption still happens solely inside
;; (submod supplemental-evidence-authority owner-frontend); this module never
;; interprets a grant and never retains owner records.
(require racket/file
         racket/string
         (only-in "../../util/credential-redaction.rkt" redact-credential-data)
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "../../runtime/approval/broker.rkt"
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-await-grant
                  cancel-approval-request!)
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         "supplemental-evidence-authority.rkt")
(provide current-supplemental-operator-publisher
         supplemental-operator-channel-available?
         supplemental-approval-presentation
         request-supplemental-approval!
         supplemental-operator-refusal)

;; Owner UI event surface (tmux/desktop/log sinks). The publisher receives
;; immutable correlation hashes only; it cannot decide, grant or present a
;; different proposal than the registered bytes.
(define current-supplemental-operator-publisher (make-parameter #f))

(define (supplemental-operator-refusal)
  "no active interactive approval channel: supplemental authorization requires \
the trusted operator frontend; batch/CLI contexts refuse honestly")

(define (supplemental-operator-channel-available?)
  (and (current-approval-channel) #t))

;; Bounded, credential-free UI copy. The FULL canonical bytes stay in the
;; registered presentation; this preview only keeps terminals readable.
(define (bounded-safe value limit)
  (define raw (redact-credential-data value))
  (define text
    (if (string? raw)
        raw
        (format "~s" raw)))
  (define compact (string-join (string-split text) " "))
  (if (> (string-length compact) limit)
      (string-append (substring compact 0 limit) "…")
      compact))

;; Full immutable presentation of the retained record, registered verbatim
;; with the broker. The record is reloaded from private storage so a caller
;; cannot present bytes that differ from what approval will bind.
(define (supplemental-approval-presentation ref)
  (define record (load-supplemental-record ref))
  (define canonical (canonical-evidence-json record))
  (define base
    (hasheq 'approval-kind
            "supplemental-evidence"
            'proposal-kind
            (hash-ref ref 'kind)
            'proposal-sha256
            (hash-ref ref 'sha256)
            'record-schema
            (hash-ref record 'schema)
            'record-canonical-json
            (string->immutable-string canonical)
            'record-preview
            (bounded-safe canonical 2000)))
  (hash-set base 'presentation-digest (sha256-string canonical)))

(define (publish-safely publisher event-type payload)
  (when (procedure? publisher)
    (with-handlers ([exn:fail? void])
      (publisher event-type payload))))

;; Register the exact proposal digest + full presentation on the ACTIVE
;; channel, emit the owner UI events, and await a REAL opaque grant.
;; Returns the grant or #f (denied / cancelled / timed out / unavailable).
;; Every refusal is an honest typed error, never a synthetic approval.
(define (request-supplemental-approval! ref [publisher (current-supplemental-operator-publisher)])
  (define channel (current-approval-channel))
  (unless channel
    (error 'supplemental-evidence-operator "~a" (supplemental-operator-refusal)))
  ;; Authenticating read first: a wrong/foreign/revoked reference never
  ;; reaches the operator UI as if it were pending.
  (define presentation (supplemental-approval-presentation ref))
  (define digest (hash-ref ref 'sha256))
  (define request-id (register-approval-request-for-channel! channel digest presentation))
  (unless request-id
    (error 'supplemental-evidence-operator
           "interactive approval channel refused registration of proposal ~a"
           digest))
  (define terminal-status "cancelled")
  (dynamic-wind void
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (e)
                                               (set! terminal-status "publisher-error")
                                               (log-warning "supplemental approval event failed: ~a"
                                                            (exn-message e)))])
                    (publish-safely publisher
                                    "supplemental-evidence.approval-requested"
                                    (hasheq 'request-id
                                            request-id
                                            'proposal-kind
                                            (hash-ref ref 'kind)
                                            'proposal-sha256
                                            digest
                                            'presentation-digest
                                            (hash-ref presentation 'presentation-digest)))
                    (define-values (outcome grant) (approval-await-grant request-id digest))
                    (set! terminal-status (symbol->string outcome))
                    (and (eq? outcome 'approved) grant)))
                (lambda ()
                  (cancel-approval-request! request-id)
                  (publish-safely publisher
                                  "supplemental-evidence.approval-terminal"
                                  (hasheq 'request-id
                                          request-id
                                          'proposal-kind
                                          (hash-ref ref 'kind)
                                          'proposal-sha256
                                          digest
                                          'terminal-status
                                          terminal-status)))))
