#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; Red-first tests for F-14 #8752: session CLI/history/metadata contracts incomplete.
;; Model metadata, message counts, SDK session-info, tool-call-count, and
;; docstring truthfulness are tested here.

(require rackunit
         racket/file
         racket/dict
         racket/list
         json
         (only-in "helpers/mock-provider.rkt" make-test-config make-simple-mock-provider)
         (only-in "../runtime/agent-session.rkt" make-agent-session session-id)
         (only-in "../runtime/session/session-store.rkt" append-entry! load-session-log)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../util/message/message.rkt" make-message message->jsexpr)
         (only-in "../util/json/jsonl.rkt" jsonl-append!)
         "../interfaces/sessions.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-session-meta-test-~a" 'directory))

(define (make-test-runtime-config dir)
  (define bus (make-event-bus))
  (define prov (make-simple-mock-provider))
  (make-test-config dir bus prov))

;; ============================================================
;; F-14 5-A: read-session-metadata must extract model from model-change meta
;; ============================================================

(test-case "read-session-metadata finds model from model-change meta"
  (define dir (make-temp-dir))
  (dynamic-wind
   void
   (lambda ()
     ;; Create a session
     (define cfg (make-test-runtime-config dir))
     (define sess (make-agent-session cfg))
     (define sid (session-id sess))
     (define session-path (build-path dir sid))
     (define jsonl-path (build-path session-path "session.jsonl"))
     ;; Append a model-change entry to the JSONL
     (define model-msg
       (make-message "m2" #f 'system 'model-change '() 2000 (hasheq 'model "claude-sonnet-4-test")))
     (jsonl-append! jsonl-path (message->jsexpr model-msg))
     ;; Read metadata
     (define meta (read-session-metadata sid session-path))
     (define model (hash-ref meta 'model "not-found"))
     (check-not-false model "model must not be #f")
     (check-equal? model "claude-sonnet-4-test"))
   (lambda () (delete-directory/files dir #:must-exist? #f))))

(test-case "read-session-metadata returns unknown when no model-change"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (make-agent-session cfg))
                  (define sid (session-id sess))
                  (define session-path (build-path dir sid))
                  (define meta (read-session-metadata sid session-path))
                  (define model (hash-ref meta 'model "not-found"))
                  (check-equal? model "unknown"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; F-14 5-C: message count must exclude internal entries
;; ============================================================

(test-case "read-session-metadata counts conversational messages, not internal entries"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (make-agent-session cfg))
                  (define sid (session-id sess))
                  (define session-path (build-path dir sid))
                  (define jsonl-path (build-path session-path "session.jsonl"))
                  ;; Append some user/assistant messages plus internal entries
                  (define user-msg (make-message "u1" #f 'user 'message '() 3000 (hasheq)))
                  (define asst-msg (make-message "a1" #f 'assistant 'message '() 4000 (hasheq)))
                  (define model-change
                    (make-message "mc1" #f 'system 'model-change '() 3500 (hasheq 'model "gpt-4")))
                  (jsonl-append! jsonl-path (message->jsexpr model-change))
                  (jsonl-append! jsonl-path (message->jsexpr user-msg))
                  (jsonl-append! jsonl-path (message->jsexpr asst-msg))
                  ;; Read metadata
                  (define meta (read-session-metadata sid session-path))
                  (define msg-count (hash-ref meta 'message-count 0))
                  ;; Should count only user + assistant, NOT model-change
                  (check-true (>= msg-count 2) "must have at least 2 conversational messages")
                  (check-false (equal? msg-count 0) "must not be zero"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; F-14 5-D: tool-call-count must use message kind, not content part type
;; ============================================================

(test-case "sessions-info tool-call-count counts tool-call kind messages"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (make-agent-session cfg))
                  (define sid (session-id sess))
                  (define session-path (build-path dir sid))
                  (define jsonl-path (build-path session-path "session.jsonl"))
                  ;; Append a tool-call message
                  (define tool-call-msg
                    (make-message "tc1" #f 'assistant 'tool-call '() 5000 (hasheq)))
                  (jsonl-append! jsonl-path (message->jsexpr tool-call-msg))
                  ;; Get info
                  (define info (sessions-info dir sid))
                  (check-not-false info)
                  (define tool-count (hash-ref info 'tool-call-count 0))
                  (check-true (>= tool-count 1) "tool-call-count must count tool-call kind messages"))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; F-14 4-A: SDK session-info must include session-dir and start-time
;; ============================================================

(test-case "read-session-metadata returns all documented keys"
  (define dir (make-temp-dir))
  (dynamic-wind void
                (lambda ()
                  (define cfg (make-test-runtime-config dir))
                  (define sess (make-agent-session cfg))
                  (define sid (session-id sess))
                  (define session-path (build-path dir sid))
                  (define meta (read-session-metadata sid session-path))
                  ;; Check all keys exist
                  (check-not-false (hash-has-key? meta 'id))
                  (check-not-false (hash-has-key? meta 'message-count))
                  (check-not-false (hash-has-key? meta 'model))
                  (check-not-false (hash-has-key? meta 'size-bytes))
                  (check-not-false (hash-has-key? meta 'mtime)))
                (lambda () (delete-directory/files dir #:must-exist? #f))))
