#lang racket/base

;; util/credential-redaction.rkt — Centralized credential redaction and leak detection
;;
;; v0.99.50 W3 (TMUX-07): The three prior redaction policies
;; (scripts/tmux-tui-explore.rkt, scripts/tmux-tui-report.rkt,
;; tests/helpers/tmux-q-harness.rkt) used inconsistent patterns that caused
;; false positives on benign substrings like `risk-score`, `set-task-state`,
;; and `Bearer authentication`.
;;
;; This module owns the single canonical policy. Pattern precision:
;; - sk-prefix keys: word-boundary + 20+ chars (OpenAI=48+, Anthropic=60+).
;;   Benign words like `risk-score` and `set-task-state` have `sk-` inside a
;;   word boundary (not at a token start), so they never match.
;; - Bearer tokens: 20+ char token. `authentication` (14 chars) is too short.
;;   Real JWTs/opaque tokens are 100+ chars.
;; - KEY=VALUE and JSON "key":"value" assignments: value must be non-empty and
;;   not already `<REDACTED>`.
;;
;; Used by: scripts/tmux-tui-explore.rkt, scripts/tmux-tui-report.rkt,
;;          tests/helpers/tmux-q-harness.rkt.

(require racket/string)

(provide REDACTED-PLACEHOLDER
         redact-secrets
         contains-secret-leak?
         find-secret-leaks)

(define REDACTED-PLACEHOLDER "<REDACTED>")

;; sk- prefix API keys: word boundary + 20+ chars.
(define rx-sk-key #px"\\bsk-[A-Za-z0-9_-]{20,}")

;; Bearer tokens: 20+ char token after "Bearer ".
(define rx-bearer #px"(?i:bearer) +[A-Za-z0-9._-]{20,}")

;; Env-style KEY=VALUE: captures key prefix (with =) + value.
(define rx-assign #px"(?i:((?:api[_-]?key|token|secret|password|credential)[ \t]*=))([^ \t\n\r<]+)")

;; JSON "key" : "value": captures and preserves the key/separator prefix.
(define rx-json-assign
  #px"(?i:((?:\"(?:api[_-]?key|token|secret|password|credential)\"[ \t]*:[ \t]*\")))([^\"<]+)")

;; Header-style API key assignments (Bearer is handled separately).
(define rx-header-assign
  #px"(?i:((?:x-api-key|api[_-]?key|token|secret|password)[ \t]*:[ \t]*))([^ \t\n\r<]{8,})")

;; Redact all secret patterns in text. Idempotent for placeholders.
(define (redact-secrets text)
  (define step1 (regexp-replace* rx-sk-key text REDACTED-PLACEHOLDER))
  (define step2 (regexp-replace* rx-bearer step1 "Bearer <REDACTED>"))
  (define step3 (regexp-replace* rx-assign step2 "\\1<REDACTED>"))
  (define step4 (regexp-replace* rx-json-assign step3 "\\1<REDACTED>"))
  (regexp-replace* rx-header-assign step4 "\\1<REDACTED>"))

(define rx-sk-key-leak #px"\\bsk-[A-Za-z0-9_-]{20,}")
(define rx-bearer-leak #px"(?i:bearer) +[A-Za-z0-9._-]{20,}")
(define rx-assign-leak
  #px"(?i:(?:api[_-]?key|token|secret|password|credential))[ \t]*=[^ \t\n\r<][^ \t\n\r]+")
(define rx-json-assign-leak
  #px"(?i:\"(?:api[_-]?key|token|secret|password|credential)\"[ \t]*:[ \t]*\"[^\"<]+)")
(define rx-header-assign-leak
  #px"(?i:(?:x-api-key|api[_-]?key|token|secret|password)[ \t]*:[ \t]*[^ \t\n\r<]{8,})")

(define leak-rx+label
  (list (cons rx-sk-key-leak "unredacted sk-prefix API key")
        (cons rx-bearer-leak "unredacted Bearer token")
        (cons rx-assign-leak "unredacted key=value assignment")
        (cons rx-json-assign-leak "unredacted JSON key:value assignment")
        (cons rx-header-assign-leak "unredacted credential header")))

(define (contains-secret-leak? text)
  (and (string? text)
       (for/or ([rl (in-list leak-rx+label)])
         (regexp-match? (car rl) text))
       #t))

(define (find-secret-leaks text)
  (if (not (string? text))
      '()
      (for/fold ([acc '()]) ([rl (in-list leak-rx+label)])
        (if (regexp-match? (car rl) text)
            (cons (cdr rl) acc)
            acc))))
