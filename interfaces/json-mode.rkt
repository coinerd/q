#lang racket

;; interfaces/json-mode.rkt — machine-readable event output
;;
;; JSON mode is a non-interactive interface that consumes the runtime
;; event stream and outputs each event as a single JSON line to stdout.
;; It is an event-to-JSON adapter.
;;
;; From ARCHITECTURE 6.2: "All external interfaces observe the system
;; through the same runtime event stream. This allows JSON mode reuse."

(require json
         "../agent/types.rkt"
         "../agent/event-bus.rkt")

(provide
 ;; Subscription management
 start-json-mode!
 stop-json-mode!

 ;; Pure functions
 event->json-line
 message->json
 json-mode-event-filter

 ;; Input parsing
 parse-json-intent
 intent
 intent?
 intent-type
 intent-payload)

;; ============================================================
;; Intent struct
;; ============================================================

(struct intent (type payload) #:transparent)
;; type    : symbol — 'prompt | 'interrupt | 'compact | 'fork | 'quit
;; payload : hash or #f

;; ============================================================
;; event->json-line — pure function
;; ============================================================

;; Convert a runtime event struct to a JSON string (one line + newline).
;; Optionally override the session-id.
(define (event->json-line evt [session-id-override #f])
  (define base-jsexpr (event->jsexpr evt))
  (define enriched
    (if session-id-override
        (hash-set base-jsexpr 'sessionId session-id-override)
        base-jsexpr))
  ;; Remove null turnId for cleaner output (optional, but keeps JSON minimal)
  (define cleaned
    (if (eq? (hash-ref enriched 'turnId 'missing) 'missing)
        enriched
        (let ([tid (hash-ref enriched 'turnId)])
          (if tid
              enriched
              (hash-remove enriched 'turnId)))))
  (string-append (jsexpr->string cleaned) "\n"))

;; ============================================================
;; message->json — pure function
;; ============================================================

;; Convert a message struct to a JSON-compatible hash using the
;; canonical serialization from types.rkt.
(define (message->json msg)
  (message->jsexpr msg))

;; ============================================================
;; json-mode-event-filter
;; ============================================================

;; Return #t for events that should be emitted in JSON mode.
;; Currently accepts all events.
(define (json-mode-event-filter evt)
  #t)

;; ============================================================
;; parse-json-intent — pure function
;; ============================================================

(define VALID-INTENTS '(prompt interrupt compact fork quit))

;; Parse a JSON line from stdin into an intent struct.
;; Returns #f on parse error or unrecognized intent.
(define (parse-json-intent line)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (cond
      [(or (not (string? line)) (string=? (string-trim line) ""))
       #f]
      [else
       (define js (string->jsexpr line))
       (define intent-str (hash-ref js 'intent #f))
       (cond
         [(not intent-str) #f]
         [(not (member (string->symbol intent-str) VALID-INTENTS)) #f]
         [else
          (define intent-sym (string->symbol intent-str))
          ;; Build payload based on intent type
          (case intent-sym
            [(prompt)
             (define text (hash-ref js 'text #f))
             (if text
                 (intent 'prompt (hasheq 'text text))
                 (intent 'prompt (hasheq)))]
            [(fork)
             (define entry-id (hash-ref js 'entryId #f))
             (if entry-id
                 (intent 'fork (hasheq 'entryId entry-id))
                 (intent 'fork (hasheq)))]
            [(interrupt compact quit)
             (intent intent-sym #f)]
            [else #f])])])))

;; ============================================================
;; start-json-mode! — subscribe and output
;; ============================================================

;; Subscribe to all events on the bus and write each as a JSON line
;; to the output port. Returns a subscription ID for unsubscribe.
(define (start-json-mode! event-bus
                          #:session-id session-id
                          #:output-port [out (current-output-port)])
  (subscribe! event-bus
              (λ (evt)
                (with-handlers ([exn:fail?
                                 (λ (exn)
                                   ;; Write an error JSON line instead of crashing
                                   (displayln
                                    (jsexpr->string
                                     (hasheq 'type "error"
                                             'error (exn-message exn)
                                             'sessionId session-id))
                                    out)
                                   (newline out))])
                  (when (json-mode-event-filter evt)
                    (display (event->json-line evt session-id) out)
                    (flush-output out))))
              #:filter (λ (_) #t)))

;; ============================================================
;; stop-json-mode! — unsubscribe
;; ============================================================

;; Cleanly unsubscribe from the event bus.
(define (stop-json-mode! event-bus subscription-id)
  (unsubscribe! event-bus subscription-id))
