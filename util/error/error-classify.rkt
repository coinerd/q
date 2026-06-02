#lang racket/base

;; util/error-classify.rkt — Failure-domain classifier
;;
;; Maps Racket exceptions to structured failure domains with user-friendly
;; messages and suggested actions. Each domain groups related error patterns.
;;
;; Domains: contract, io, network, provider, session, tool, tui
;; Returns (list domain user-message suggestions) or #f if no classification.

(require racket/contract
         racket/string)

(provide (contract-out [classify-error (-> exn? (or/c (list/c symbol? string? (listof string?)) #f))]
                       [error-domain? (-> any/c boolean?)]
                       [error-domains (-> (listof symbol?))]))

;; Domain registry
(define error-domain-list '(contract io network provider session tool tui))

(define (error-domain? v)
  (and (symbol? v) (memq v error-domain-list) #t))

(define (error-domains)
  error-domain-list)

;; classify-error : exn? -> (or/c (list/c symbol? string? (listof string?)) #f)
;;
;; Maps common Racket exception patterns to a failure domain, a user-friendly
;; message, and a list of suggested actions. Returns #f when no pattern matches.

(define (classify-error e)
  (define msg (exn-message e))
  (cond
    ;; ── Contract domain ────────────────────────────────────
    [(or (regexp-match? #rx"contract" msg)
         (regexp-match? #rx"blaming:" msg)
         (regexp-match? #rx"expected:" msg)
         (regexp-match? #rx"given:" msg))
     (list 'contract
           "A contract violation occurred."
           '("This is usually an internal error, not a user mistake."
             "Try restarting the session."
             "If this persists, please file a bug report."))]

    ;; ── Network domain ─────────────────────────────────────
    [(or (regexp-match? #rx"connection refused" msg)
         (regexp-match? #rx"connection timed? ?out" msg)
         (regexp-match? #rx"host not found" msg)
         (regexp-match? #rx"network" msg)
         (regexp-match? #rx"SSL|certificate" msg))
     (list
      'network
      (cond
        [(regexp-match? #rx"SSL|certificate" msg) "SSL/TLS certificate error when connecting to API."]
        [(regexp-match? #rx"connection refused" msg) "Could not connect to the API server."]
        [else "A network error occurred."])
      '("Check your internet connection." "If using a local model, make sure the server is running."
                                          "Check your system's SSL certificates."))]

    ;; ── Provider domain ────────────────────────────────────
    [(or (regexp-match? #rx"API key" msg)
         (regexp-match? #rx"unauthorized|401" msg)
         (regexp-match? #rx"403" msg)
         (regexp-match? #rx"rate.limit|429" msg)
         (regexp-match? #rx"500|502|503" msg)
         (regexp-match? #rx"provider" msg))
     (list
      'provider
      (cond
        [(or (regexp-match? #rx"API key" msg)
             (regexp-match? #rx"unauthorized|401" msg)
             (regexp-match? #rx"403" msg))
         "API authentication failed."]
        [(regexp-match? #rx"rate.limit|429" msg) "API rate limit reached."]
        [(or (regexp-match? #rx"500|502|503" msg) (regexp-match? #rx"provider" msg))
         "The API provider returned an error."]
        [else "A provider error occurred."])
      '("Check your API key in ~/.q/config.json."
        "Set the appropriate environment variable (OPENAI_API_KEY, ANTHROPIC_API_KEY, or GEMINI_API_KEY)."
        "Run 'q init' for guided setup."
        "Wait a moment and try again if rate-limited."))]

    ;; ── I/O domain ─────────────────────────────────────────
    [(or (regexp-match? #rx"file.*not found|no such file" msg)
         (regexp-match? #rx"permission denied" msg)
         (regexp-match? #rx"read-json:" msg)
         (regexp-match? #rx"open-input-file" msg))
     (list 'io
           (cond
             [(regexp-match? #rx"read-json:" msg) "The configuration file contains invalid JSON."]
             [(regexp-match? #rx"permission denied" msg)
              "Permission denied when accessing a file or directory."]
             [(regexp-match? #rx"file.*not found|no such file" msg) "A required file was not found."]
             [else "An I/O error occurred."])
           '("Check the file path and try again."
             "Run 'q init' to set up your configuration."
             "Check file permissions."
             "Run 'q doctor' to diagnose configuration issues."))]

    ;; ── Session domain ─────────────────────────────────────
    [(or (regexp-match? #rx"hash-ref:" msg) (regexp-match? #rx"session" msg))
     (list 'session
           (cond
             [(regexp-match? #rx"hash-ref:" msg) "A required value was missing from the data."]
             [else "A session error occurred."])
           '("Check your configuration file for missing fields."
             "Run 'q doctor' to diagnose configuration issues."
             "Try creating a new session."))]

    ;; ── Tool domain ────────────────────────────────────────
    [(or (regexp-match? #rx"tool.*timeout|timed? ?out" msg)
         (regexp-match? #rx"tool.*error" msg)
         (regexp-match? #rx"subprocess" msg)
         (regexp-match? #rx"sandbox" msg))
     (list 'tool
           "A tool execution error occurred."
           '("The tool may have timed out or encountered an error."
             "Try running the command again."
             "Check tool permissions with 'q doctor'."))]

    ;; ── TUI domain ─────────────────────────────────────────
    [(or (regexp-match? #rx"terminal" msg)
         (regexp-match? #rx"render" msg)
         (regexp-match? #rx"display" msg)
         (regexp-match? #rx"tty" msg))
     (list 'tui
           "A terminal rendering error occurred."
           '("Try resizing your terminal."
             "Check that your terminal supports the required escape sequences."
             "Try running in non-TUI mode with 'q --no-tui'."))]

    [else #f]))
