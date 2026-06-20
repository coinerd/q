#lang racket/base

;; sandbox/subprocess-helpers.rkt — Pure result-boundary helpers
;;
;; W3 v0.99.35: Extracted from subprocess.rkt to create a clean boundary
;; between pure result construction/classification and effectful subprocess
;; management (process spawning, port reading, custodian cleanup).
;;
;; All functions in this module are pure (no I/O, no mutation, no parameters).
;; They can be tested in isolation without spawning real subprocesses.
;;
;; Boundary contract:
;;   INPUTS:  Plain data (strings, integers, booleans, regex patterns)
;;   OUTPUTS: Plain data (subprocess-result struct, strings, booleans)
;;   EFFECTS: None — safe to call from any context

(require racket/format
         racket/string)

;; Struct (re-exported by subprocess.rkt for backward compat)
(provide subprocess-result
         subprocess-result?
         subprocess-result-exit-code
         subprocess-result-stdout
         subprocess-result-stderr
         subprocess-result-timed-out?
         subprocess-result-elapsed-ms
         subprocess-result-truncated?
         ;; Exit code constants
         EXIT-CODE-TIMEOUT
         EXIT-CODE-ERROR
         ;; Pure result constructors
         make-error-result
         make-timeout-result
         make-success-result
         ;; Pure result classifiers
         exit-success?
         exit-timeout?
         exit-error?
         ;; Pure message formatters
         format-timeout-message
         format-truncation-message
         ;; Secret detection (pure — takes explicit args, no parameters)
         secret-patterns
         SECRET-IMPLICIT-ALLOWLIST
         check-secret-var?)

;; ============================================================
;; Result struct
;; ============================================================

(struct subprocess-result
        (exit-code ; integer
         stdout ; string
         stderr ; string
         timed-out? ; boolean
         elapsed-ms ; number
         truncated?) ; boolean — output was cut at byte budget
  #:transparent)

;; ============================================================
;; Exit code constants
;; ============================================================

;; Exit code used when a subprocess is killed due to timeout.
(define EXIT-CODE-TIMEOUT -9)

;; Exit code used when subprocess execution failed (e.g., command not found).
(define EXIT-CODE-ERROR -1)

;; ============================================================
;; Pure result constructors
;; ============================================================

;; Construct an execution-error result (e.g., command not found, exec failure).
(define (make-error-result message elapsed-ms)
  (subprocess-result EXIT-CODE-ERROR "" message #f elapsed-ms #f))

;; Construct a timeout result with partial output captured before the kill.
;; Appends the timeout system message to partial stderr.
(define (make-timeout-result stdout stderr timeout-secs elapsed-ms truncated?)
  (subprocess-result EXIT-CODE-TIMEOUT
                     stdout
                     (string-append stderr (format-timeout-message timeout-secs))
                     #t
                     elapsed-ms
                     truncated?))

;; Construct a normal completion result.
(define (make-success-result exit-code stdout stderr elapsed-ms truncated?)
  (subprocess-result exit-code stdout stderr #f elapsed-ms truncated?))

;; ============================================================
;; Pure result classifiers
;; ============================================================

(define (exit-success? exit-code)
  (eqv? exit-code 0))
(define (exit-timeout? exit-code)
  (eqv? exit-code EXIT-CODE-TIMEOUT))
(define (exit-error? exit-code)
  (eqv? exit-code EXIT-CODE-ERROR))

;; ============================================================
;; Pure message formatters
;; ============================================================

(define (format-timeout-message timeout-secs)
  (format "\n[SYS] Command timed out after ~a seconds. Partial output shown above." timeout-secs))

(define (format-truncation-message max-bytes)
  (format "\n[output truncated at ~a bytes]" max-bytes))

;; ============================================================
;; Secret environment variable detection (pure — no parameters)
;; ============================================================

;; Patterns that indicate sensitive env vars.
(define secret-patterns
  (list #rx"(?i:API.?KEY)"
        #rx"(?i:SECRET)"
        #rx"(?i:TOKEN)"
        #rx"(?i:PASSWORD)"
        #rx"(?i:CREDENTIAL)"
        #rx"(?i:^AUTH$|^AUTH_|_AUTH_)"
        #rx"(?i:GH_PAT)"
        #rx"(?i:_PAT$)"))

;; Built-in implicit allowlist: well-known non-secret env vars that should
;; never be scrubbed even if they partially match a secret pattern.
(define SECRET-IMPLICIT-ALLOWLIST '("XAUTHORITY" "GPG_AUTH_INFO" "AUTHOR" "GPG_TTY" "SSH_AUTH_SOCK"))

;; Pure secret detection — given a variable name, extra denylist patterns,
;; and an allowlist, determine if the variable should be scrubbed.
;;
;; Priority: implicit-allowlist > user-allowlist > (default-patterns + extra-denylist)
;;
;; This is the pure core of secret-env-var? in subprocess.rkt. That function
;; wraps this with parameter reads for current-secret-scrub-denylist and
;; current-secret-scrub-allowlist.
(define (check-secret-var? name extra-denylist extra-allowlist)
  (cond
    ;; 1. Implicit allowlist — always safe
    [(member name SECRET-IMPLICIT-ALLOWLIST) #f]
    ;; 2. User allowlist — explicitly allowed
    [(for/or ([pat (in-list extra-allowlist)])
       (regexp-match? pat name))
     #f]
    ;; 3. Default patterns + extra denylist
    [else
     (for/or ([pat (in-list (append secret-patterns extra-denylist))])
       (regexp-match? pat name))]))
