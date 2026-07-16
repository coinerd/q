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
         redact-credential-data
         sensitive-credential-key?
         contains-secret-leak?
         find-secret-leaks
         redact-credential-jsexpr)

(define REDACTED-PLACEHOLDER "<REDACTED>")

(define credential-key-pattern
  (string-append "(?:api[_-]?key|access[_-]?token|refresh[_-]?token|client[_-]?secret|"
                 "capability[_-]?(?:token|secret)|code[_-]?verifier|authorization[_-]?code|"
                 "token|secret|password|credential)"))

(define rx-sk-key #px"\\bsk-[A-Za-z0-9_-]{20,}")
(define rx-bearer #px"(?i:bearer) +[A-Za-z0-9._-]{20,}")
(define rx-authorization-bearer
  #px"(?i:(Authorization[ ]*:[ ]*Bearer[ ]+))(?!authentication(?:[ ]|$))([^ \n\r<]+)")
(define rx-assign (pregexp (format "(?i:((?:~a)\\s*=))([^\\s<]+)" credential-key-pattern)))
(define rx-json-assign
  (pregexp (format "(?i:((?:\\\"(?:~a)\\\"\\s*:\\s*\\\")))([^\\\"<]+)" credential-key-pattern)))
(define rx-header-assign
  (pregexp (format "(?i:((?:x-api-key|~a)\\s*:\\s*))([^\\s<]+)" credential-key-pattern)))

(define (redact-secrets text)
  (unless (string? text)
    (raise-argument-error 'redact-secrets "string?" text))
  (define step1 (regexp-replace* rx-sk-key text REDACTED-PLACEHOLDER))
  (define step2 (regexp-replace* rx-authorization-bearer step1 "\\1<REDACTED>"))
  (define step3 (regexp-replace* rx-bearer step2 "Bearer <REDACTED>"))
  (define step4 (regexp-replace* rx-assign step3 "\\1<REDACTED>"))
  (define step5 (regexp-replace* rx-json-assign step4 "\\1<REDACTED>"))
  (regexp-replace* rx-header-assign step5 "\\1<REDACTED>"))

(define sensitive-key-names
  '("apikey" "xapikey"
             "xgoogapikey"
             "token"
             "accesstoken"
             "refreshtoken"
             "oauthtoken"
             "bearertoken"
             "capabilitytoken"
             "capabilitysecret"
             "clientsecret"
             "secret"
             "password"
             "credential"
             "authorization"
             "codeverifier"
             "authorizationcode"))

(define (key->comparison-string key)
  (cond
    [(string? key) key]
    [(symbol? key) (symbol->string key)]
    [(keyword? key) (keyword->string key)]
    [else #f]))

(define (sensitive-credential-key? key)
  (define raw (key->comparison-string key))
  (and raw (member (regexp-replace* #px"[^a-z0-9]" (string-downcase raw) "") sensitive-key-names) #t))

(define (redact-credential-data value)
  (cond
    [(hash? value)
     (for/hash ([(key child) (in-hash value)])
       (values key
               (if (sensitive-credential-key? key)
                   REDACTED-PLACEHOLDER
                   (redact-credential-data child))))]
    [(vector? value)
     (vector->immutable-vector (for/vector ([child (in-vector value)])
                                 (redact-credential-data child)))]
    [(list? value) (map redact-credential-data value)]
    [(pair? value) (cons (redact-credential-data (car value)) (redact-credential-data (cdr value)))]
    [(string? value) (redact-secrets value)]
    [(struct? value) "<redacted-struct>"]
    [else value]))

;; Convert a redacted value to a write-json-safe shape. String and keyword
;; object keys become uninterned symbols so untrusted names do not grow the
;; process-wide symbol table.
(define (redact-credential-jsexpr value)
  (define clean (redact-credential-data value))
  (let convert ([current clean])
    (cond
      [(hash? current)
       (for/hasheq ([(key child) (in-hash current)])
         (define json-key
           (cond
             [(symbol? key) key]
             [(string? key) (string->uninterned-symbol key)]
             [(keyword? key) (string->uninterned-symbol (keyword->string key))]
             [else (string->uninterned-symbol (format "~a" key))]))
         (values json-key (convert child)))]
      [(vector? current) (map convert (vector->list current))]
      [(list? current) (map convert current)]
      [(pair? current) (list (convert (car current)) (convert (cdr current)))]
      [(eq? current 'null) 'null]
      [(symbol? current) (symbol->string current)]
      [(keyword? current) (keyword->string current)]
      [(or (string? current) (number? current) (boolean? current)) current]
      [else (format "<unsupported:~a>" current)])))

(define rx-sk-key-leak #px"\\bsk-[A-Za-z0-9_-]{20,}")
(define rx-bearer-leak #px"(?i:bearer) +[A-Za-z0-9._-]{20,}")
(define rx-authorization-bearer-leak
  #px"(?i:Authorization[ ]*:[ ]*Bearer[ ]+)(?!authentication(?:[ ]|$))[^ \n\r<]+")
(define rx-assign-leak (pregexp (format "(?i:(?:~a))\\s*=[^\\s<][^\\s]*" credential-key-pattern)))
(define rx-json-assign-leak
  (pregexp (format "(?i:\\\"(?:~a)\\\"\\s*:\\s*\\\"[^\\\"<]+)" credential-key-pattern)))
(define rx-header-assign-leak
  (pregexp (format "(?i:(?:x-api-key|~a))\\s*:\\s*[^\\s<]+" credential-key-pattern)))

(define leak-rx+label
  (list (cons rx-sk-key-leak "unredacted sk-prefix API key")
        (cons rx-bearer-leak "unredacted Bearer token")
        (cons rx-authorization-bearer-leak "unredacted Authorization Bearer token")
        (cons rx-assign-leak "unredacted key=value assignment")
        (cons rx-json-assign-leak "unredacted JSON key:value assignment")
        (cons rx-header-assign-leak "unredacted credential header")))

(define (contains-secret-leak? text)
  (and (string? text)
       (for/or ([entry (in-list leak-rx+label)])
         (regexp-match? (car entry) text))
       #t))

(define (find-secret-leaks text)
  (if (not (string? text))
      '()
      (for/fold ([found '()]) ([entry (in-list leak-rx+label)])
        (if (regexp-match? (car entry) text)
            (cons (cdr entry) found)
            found))))
