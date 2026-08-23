#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; q/tests/test-gui-thinking-artifact-parity.rkt — W2 verification
;;
;; Wave W2 Done criteria:
;;  1. GUI retains FULL reasoning bodies (>200 chars) with artifact id and
;;     lifecycle under all event orderings (stream.completed, turn.completed,
;;     duplicate/stale completions).
;;  2. GUI Ctrl+O intent + visible disclosure control both toggle the shared
;;     disclosure state; target resolution matches TUI semantics
;;     (explicit -> active streaming -> most recent candidate).
;;  3. No destructive truncation remains in gui/state-sync.rkt.

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         racket/port
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/disclosure-state.rkt"
         "../ui-core/conversation-reducer.rkt"
         "../ui-core/ui-intents.rkt"
         "../ui-core/theme-protocol.rkt"
         "../util/event/event.rkt")

(define (mk-event tag payload [session-id "gui-session"] [turn-id "gui-turn"])
  (make-event tag (current-inexact-milliseconds) session-id turn-id payload))

(define (fresh-box)
  (box (make-gui-state)))

(define (thinking-messages gs)
  (filter (lambda (m) (eq? (gui-message-kind m) 'thinking)) (gui-state-messages gs)))

(define (thinking-artifact gs)
  (define msgs (thinking-messages gs))
  (and (pair? msgs) (hash-ref (gui-message-meta (car msgs)) 'artifact #f)))

(define sync-src
  (with-input-from-file (build-path (or (current-load-relative-directory) (current-directory))
                                    "../gui/state-sync.rkt")
                        (lambda () (port->bytes (current-input-port)))))

(define test-full-body-retention
  (test-suite "full reasoning body retention"
    (test-case "thinking >200 chars fully retained after model.stream.completed"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 500 #\R))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
      (sub (mk-event "model.stream.completed" (hash)))
      (define gs (unbox sb))
      (define artifact (thinking-artifact gs))
      (check-not-false artifact "thinking artifact must exist")
      (check-equal? (conversation-artifact-body artifact) long-thinking)
      (check-equal? (conversation-artifact-kind artifact) 'thinking)
      (check-equal? (conversation-artifact-lifecycle artifact) 'completed)
      (check-not-false (conversation-artifact-id artifact))
      (check-equal? (hash-ref (gui-message-meta (car (thinking-messages gs))) 'artifact-id)
                    (conversation-artifact-id artifact)))

    (test-case "turn.completed after stream.completed does not reset the artifact"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 400 #\Q))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub (mk-event "turn.completed" (hash)))
      (define artifact (thinking-artifact (unbox sb)))
      (check-not-false artifact)
      (check-equal? (conversation-artifact-body artifact) long-thinking)
      (check-equal? (conversation-artifact-lifecycle artifact) 'completed))

    (test-case "turn.completed without stream.completed still finalizes full body"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 320 #\Z))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
      (sub (mk-event "turn.completed" (hash)))
      (define artifact (thinking-artifact (unbox sb)))
      (check-not-false artifact)
      (check-equal? (conversation-artifact-body artifact) long-thinking))

    (test-case "duplicate stream.completed events never truncate the body"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 350 #\D))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub (mk-event "model.stream.completed" (hash)))
      (define artifact (thinking-artifact (unbox sb)))
      (check-equal? (conversation-artifact-body artifact) long-thinking)
      (check-equal? (length (thinking-messages (unbox sb))) 1))

    (test-case "stale-turn completion cannot touch the active artifact"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 280 #\S))
      (sub (mk-event "turn.started" (hash) "gui-session" "live-turn"))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking) "gui-session" "live-turn"))
      (sub (mk-event "model.stream.completed" (hash) "gui-session" "stale-turn"))
      (sub (mk-event "turn.completed" (hash) "gui-session" "stale-turn"))
      (define artifact (thinking-artifact (unbox sb)))
      (check-equal? (conversation-artifact-body artifact) long-thinking))))

(define test-shared-state-fields
  (test-suite "gui-state carries shared reducer + disclosure state"
    (test-case "gui-state embeds reducer-state (conversation-reducer)"
      (check-true (reducer-state? (gui-state-conversation-reducer (make-gui-state))))
      (check-true (reducer-state? (gui-state-conversation-reducer (gui-state-set-conversation-reducer
                                                                   (make-gui-state)
                                                                   (make-reducer-state))))))

    (test-case "gui-state embeds disclosure-state and toggling updates it"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta (make-string 260 #\T))))
      (sub (mk-event "model.stream.completed" (hash)))
      (define id (gui-state-latest-thinking-artifact-id (unbox sb)))
      (check-not-false id)
      (check-true (disclosure-state? (gui-state-disclosure (unbox sb))))
      (check-false (disclosure-expanded? (gui-state-disclosure (unbox sb)) id))
      (define toggled (gui-state-apply-intent (unbox sb) (make-toggle-detail-intent id)))
      (check-true (disclosure-expanded? (gui-state-disclosure toggled) id)
                  "toggle must hit shared state"))))

(define test-ctrl-o-target-resolution
  (test-suite "Ctrl+O target resolution matches TUI semantics"
    (test-case "explicit target wins over active streaming artifact"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\A))))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub
       (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\B)) "gui-session" "turn-2"))
      ;; 'turn-2' artifact is still streaming
      (define streaming-id
        (for/first ([m (in-list (reverse (gui-state-messages (unbox sb))))]
                    #:when (and (eq? (gui-message-kind m) 'thinking)
                                (eq? (conversation-artifact-lifecycle (hash-ref (gui-message-meta m)
                                                                                'artifact))
                                     'streaming)))
          (hash-ref (gui-message-meta m) 'artifact-id)))
      (check-not-false streaming-id)
      (define completed-id
        (gui-state-latest-thinking-artifact-id
         (struct-copy gui-state
                      (unbox sb)
                      [messages
                       (filter (lambda (m)
                                 (not (equal? (hash-ref (gui-message-meta m) 'artifact-id #f)
                                              streaming-id)))
                               (gui-state-messages (unbox sb)))])))
      (define toggled (gui-state-apply-intent (unbox sb) (make-toggle-detail-intent completed-id)))
      (check-true (disclosure-expanded? (gui-state-disclosure toggled) completed-id))
      (check-false (disclosure-expanded? (gui-state-disclosure toggled) streaming-id)))

    (test-case "active streaming artifact wins when no explicit target"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\A))))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub
       (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\B)) "gui-session" "turn-2"))
      (define streaming-id
        (for/first ([m (in-list (reverse (gui-state-messages (unbox sb))))]
                    #:when (and (eq? (gui-message-kind m) 'thinking)
                                (eq? (conversation-artifact-lifecycle (hash-ref (gui-message-meta m)
                                                                                'artifact))
                                     'streaming)))
          (hash-ref (gui-message-meta m) 'artifact-id)))
      ;; no explicit target — resolver must pick the streaming artifact, not the older completed one
      (define toggled (gui-state-apply-intent (unbox sb) (make-toggle-detail-intent)))
      (check-true (disclosure-expanded? (gui-state-disclosure toggled) streaming-id))
      (define older-ids
        (for/list ([m (in-list (thinking-messages (unbox sb)))]
                   #:unless (equal? (hash-ref (gui-message-meta m) 'artifact-id) streaming-id))
          (hash-ref (gui-message-meta m) 'artifact-id)))
      (for ([id (in-list older-ids)])
        (check-false (disclosure-expanded? (gui-state-disclosure toggled) id))))

    (test-case "most recent candidate wins when nothing is streaming"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\X))))
      (sub (mk-event "model.stream.completed" (hash)))
      (sub
       (mk-event "model.stream.thinking" (hash 'delta (make-string 210 #\Y)) "gui-session" "turn-2"))
      (sub (mk-event "model.stream.completed" (hash) "gui-session" "turn-2"))
      (define ids
        (for/list ([m (in-list (thinking-messages (unbox sb)))])
          (hash-ref (gui-message-meta m) 'artifact-id)))
      (check-equal? (length ids) 2)
      (define toggled (gui-state-apply-intent (unbox sb) (make-toggle-detail-intent)))
      (check-true (disclosure-expanded? (gui-state-disclosure toggled) (second ids))
                  "latest candidate must be toggled")
      (check-false (disclosure-expanded? (gui-state-disclosure toggled) (first ids))))

    (test-case "Ctrl+O toggles full body on and off through shared intent"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (define long-thinking (make-string 250 #\F))
      (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
      (sub (mk-event "model.stream.completed" (hash)))
      (define id (gui-state-latest-thinking-artifact-id (unbox sb)))
      (define expanded (gui-state-apply-intent (unbox sb) (make-toggle-detail-intent id)))
      (check-true (string-prefix? (gui-message-text (car (thinking-messages expanded))) long-thinking)
                  "expanded entry shows the FULL body")
      (define collapsed (gui-state-apply-intent expanded (make-toggle-detail-intent id)))
      (check-true (string-contains? (gui-message-text (car (thinking-messages collapsed))) "Ctrl+O")
                  "collapsed entry shows the disclosure hint")
      (check-true (<= (string-length (gui-message-text (car (thinking-messages collapsed)))) 260)
                  "collapsed entry stays a short preview")
      ;; full body survives folding in the artifact metadata
      (check-equal? (conversation-artifact-body (thinking-artifact collapsed)) long-thinking))))

(define test-visible-disclosure-control
  (test-suite "visible disclosure control on thinking entries"
    (test-case "rendered thinking descriptor carries a disclosure action intent"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta (make-string 300 #\V))))
      (sub (mk-event "model.stream.completed" (hash)))
      (define m (car (thinking-messages (unbox sb))))
      (define descriptor (render-message-descriptor (gui-message->hash m) (default-theme)))
      (define action-segment
        (findf (lambda (segment) (eq? (hash-ref segment 'type #f) 'disclosure-action))
               (hash-ref descriptor 'segments)))
      (check-not-false action-segment "thinking entry must expose a clickable disclosure control")
      (check-equal? (ui-intent-target (hash-ref action-segment 'intent))
                    (hash-ref (gui-message-meta m) 'artifact-id)))))

(define test-no-destructive-truncation
  (test-suite "no destructive truncation in gui/state-sync.rkt"
    (test-case "no 200-char thinking summary remains"
      ;; The old bug destructively rewrote the thinking body to a <=200-char
      ;; summary inside state-sync.rkt.  That code must stay gone.
      (check-false (regexp-match? #rx#"(?i:thinking).{0,80}substring" sync-src))
      (check-false (regexp-match? #rx#"summary" sync-src)))
    (test-case "artifact body is stored, not a substring preview"
      (check-not-false (regexp-match? #rx#"conversation-artifact-body" sync-src)))))

(module+ test
  (run-tests (test-suite "gui-thinking-artifact-parity"
               test-full-body-retention
               test-shared-state-fields
               test-ctrl-o-target-resolution
               test-visible-disclosure-control
               test-no-destructive-truncation)))

(module+ main
  (run-tests (test-suite "gui-thinking-artifact-parity"
               test-full-body-retention
               test-shared-state-fields
               test-ctrl-o-target-resolution
               test-visible-disclosure-control
               test-no-destructive-truncation)))
