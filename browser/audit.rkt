#lang racket

;; browser/audit.rkt — JSONL audit trail for all browser actions

(require json
         "types.rkt"
         (only-in "../agent/event-bus.rkt" event-bus?)
         (only-in "../agent/event-emitter.rkt" emit-session-event!))

(provide
 log-browser-action!
 emit-browser-event!
 audit-log-path)

(define (audit-log-path artifact-dir)
  (build-path artifact-dir "logs" "browser-actions.jsonl"))

(define (log-browser-action! session-id action-type result artifact-dir)
  (when artifact-dir
    (define log-file (audit-log-path artifact-dir))
    (define dir (path-only log-file))
    (when dir
      (unless (directory-exists? dir)
        (make-directory* dir)))
    (define entry (hasheq 'timestamp (current-milliseconds)
                          'session-id session-id
                          'action (symbol->string action-type)
                          'result (format-result result)))
    (call-with-output-file log-file
      (lambda (out)
        (displayln (jsexpr->string entry) out))
      #:exists 'append)))

(define (format-result result)
  (cond
    [(browser-observation? result)
     (hasheq 'type "observation"
             'url (browser-observation-url result)
             'title (browser-observation-title result))]
    [(eq? result 'ok) (hasheq 'type "ok")]
    [(hash? result) result]
    [else (format "~a" result)]))

(define (emit-browser-event! bus event-type session-id payload)
  (when (and bus (event-bus? bus))
    (emit-session-event! bus
                         session-id
                         (if (symbol? event-type)
                             (symbol->string event-type)
                             event-type)
                         (if (hash? payload) payload (hasheq)))))
