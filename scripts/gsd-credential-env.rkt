#lang racket/base

;; scripts/gsd-credential-env.rkt — the credential boundary for delivery
;; subprocesses, extracted from extensions/gsd/delivery-handoff.rkt so that the
;; scripts-level delivery seams do not depend on an extension module (that
;; inversion made the wave-integrity rehearsal's scratch root unable to load
;; scripts/gsd-delivery-git.rkt).
;;
;; Contract (unchanged): every delivery subprocess runs with a scrubbed
;; environment plus ONLY the credential variables named here, re-added for that
;; one process. Tokens never reach worker prompts, campaign messages or refusal
;; text; `redact-delivery-text` (delivery-handoff) is the text-side guard.
(provide controller-environment
         CREDENTIAL-ENV-NAMES)

(require "../sandbox/subprocess.rkt")

(define CREDENTIAL-ENV-NAMES '("GH_TOKEN" "GITHUB_TOKEN"))

(define (controller-environment)
  (define incoming (current-environment-variables))
  (define env (sanitize-env))
  (for ([name (in-list CREDENTIAL-ENV-NAMES)])
    (define value (environment-variables-ref incoming (string->bytes/utf-8 name)))
    (when value
      (environment-variables-set! env (string->bytes/utf-8 name) value)))
  env)
