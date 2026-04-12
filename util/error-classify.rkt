#lang racket/base

;; util/error-classify.rkt — maps Racket exceptions to user-friendly messages
;;
;; Issue #165: Classify common Racket exceptions into user-friendly messages.
;; Returns (cons user-message suggestions) or #f if no classification matches.

(provide classify-error)

;; classify-error : exn? -> (or/c (cons/c string? (listof string?)) #f)
;;
;; Maps common Racket exception patterns to a user-friendly message and
;; a list of suggested actions. Returns #f when no pattern matches.

(define (classify-error e)
  (define msg (exn-message e))
  (cond
    [(regexp-match? #rx"hash-ref:" msg)
     (cons "A required value was missing from the data."
           '("Check your configuration file for missing fields."
             "Run 'q doctor' to diagnose configuration issues."))]
    [(regexp-match? #rx"read-json:" msg)
     (cons "The configuration file contains invalid JSON."
           '("Check ~/.q/config.json for syntax errors."
             "Run 'q doctor' to validate your configuration."))]
    [(regexp-match? #rx"connection refused" msg)
     (cons "Could not connect to the API server."
           '("Check your internet connection."
             "If using a local model, make sure the server is running."))]
    [(regexp-match? #rx"SSL|certificate" msg)
     (cons "SSL/TLS certificate error when connecting to API."
           '("Check your system's SSL certificates." "Try updating your Racket installation."))]
    [(regexp-match? #rx"file.*not found|no such file" msg)
     (cons "A required file was not found."
           '("Check the file path and try again." "Run 'q init' to set up your configuration."))]
    [(regexp-match? #rx"permission denied" msg)
     (cons "Permission denied when accessing a file or directory."
           '("Check file permissions." "Try running with appropriate access rights."))]
    [(or (regexp-match? #rx"API key" msg)
         (regexp-match? #rx"unauthorized|401" msg)
         (regexp-match? #rx"403" msg))
     (cons
      "API authentication failed."
      '("Check your API key in ~/.q/config.json."
        "Set the appropriate environment variable (OPENAI_API_KEY, ANTHROPIC_API_KEY, or GEMINI_API_KEY)."
        "Run 'q init' for guided setup."))]
    [(regexp-match? #rx"rate.limit|429" msg)
     (cons "API rate limit reached."
           '("Wait a moment and try again." "Consider upgrading your API plan for higher limits."))]
    [else #f]))
