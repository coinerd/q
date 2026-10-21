#lang racket/base
(require racket/string
         "../util/credential-redaction.rkt")
(provide safe-denial-reason)

;; No arbitrary payload printing, including exceptions. Redact before truncating
;; so a credential spanning the boundary cannot leave a visible prefix.
(define (safe-denial-reason reason [fallback "authorization denied (no safe reason supplied)"])
  (cond
    [(not (string? reason)) fallback]
    [else
     (define no-ansi
       (regexp-replace* #px"\u001b(?:\\[[0-?]*[ -/]*[@-~]|\\][^\u0007\u001b]*(?:\u0007|\u001b\\\\))"
                        reason
                        ""))
     (define clean
       (string-trim (regexp-replace* #px"[\u0000-\u001f\u007f-\u009f\u202a-\u202e\u2066-\u2069]"
                                     (redact-secrets no-ansi)
                                     " ")))
     (cond
       [(string=? clean "") fallback]
       [(> (string-length clean) 1024) (string-append (substring clean 0 1021) "...")]
       [else clean])]))
