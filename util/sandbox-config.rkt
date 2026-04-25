#lang racket/base

;; util/sandbox-config.rkt — Pure sandbox configuration readers
;;
;; Reads sandbox settings from a q-settings? struct or a plain hash.
;; Extracted from runtime/settings.rkt so that tools/builtins/ can
;; access sandbox config without importing from the runtime layer (ARCH-03).
;;
;; When passed a transparent q-settings? struct, extracts the merged hash
;; (field index 2) via vector-ref. When passed a hash, uses it directly.

(provide sandbox-enabled?
         sandbox-timeout
         sandbox-memory-limit
         sandbox-max-output
         sandbox-max-processes)

;; ── Internal: extract merged hash ──────────────────────────

;; q-settings is a transparent struct with fields: global, project, merged.
;; Field index 2 = merged hash. For transparent structs, we can use
;; struct->vector to access fields without importing the struct definition.
(define (get-merged-hash v)
  (cond
    [(hash? v) v]
    [(struct? v) (vector-ref (struct->vector v) 3)] ; index 0 is struct name
    [else (hash)]))

;; ── Internal: nested hash ref ──────────────────────────────

(define NOT-FOUND (gensym 'not-found))

(define (hash-nested-ref h key-path)
  (cond
    [(null? key-path) NOT-FOUND]
    [(null? (cdr key-path)) (hash-ref h (car key-path) NOT-FOUND)]
    [else
     (define next (hash-ref h (car key-path) NOT-FOUND))
     (if (hash? next)
         (hash-nested-ref next (cdr key-path))
         NOT-FOUND)]))

(define (ref* h key-path [default #f])
  (define result (hash-nested-ref h key-path))
  (if (eq? result NOT-FOUND) default result))

;; ── Public API ─────────────────────────────────────────────

(define (sandbox-enabled? settings)
  (ref* (get-merged-hash settings) '(tools use-sandbox) #t))

(define (sandbox-timeout settings)
  (ref* (get-merged-hash settings) '(tools sandbox-timeout) 120))

(define (sandbox-memory-limit settings)
  (ref* (get-merged-hash settings) '(tools sandbox-memory) 536870912))

(define (sandbox-max-output settings)
  (ref* (get-merged-hash settings) '(tools sandbox-max-output) 1048576))

(define (sandbox-max-processes settings)
  (ref* (get-merged-hash settings) '(tools sandbox-max-processes) 10))
