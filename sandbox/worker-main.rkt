#lang racket/base

;; sandbox/worker-main.rkt — Execution Plane Worker (stdio)
;;
;; Run as: racket -tm sandbox/worker-main.rkt
;; Communicates via newline-delimited JSON on stdin/stdout.
;;
;; Protocol:
;;   1. Read a line from stdin (newline-delimited JSON)
;;   2. Parse as ipc-request
;;   3. Dispatch to worker-tool-registry
;;   4. Write ipc-response as JSON to stdout
;;   5. Repeat until EOF on stdin
;;
;; W2 (v0.99.12): Dispatch logic extracted to worker-dispatch.rkt.
;; This module is now a thin stdio wrapper around process-request-line
;; and serialize-response.

(require racket/string
         "ipc-protocol.rkt"
         "worker-dispatch.rkt")

;; ── Worker Main Loop ────────────────────────────────────────────

(define (worker-loop)
  (define line (read-line (current-input-port) 'any))
  (cond
    ;; Gateway closed stdin → clean exit
    [(eof-object? line) (exit 0)]
    [else
     (define trimmed (string-trim line))
     (cond
       ;; Empty line — skip, don't process
       [(string=? trimmed "") (worker-loop)]
       [else
        ;; Process the request
        (define response (process-request-line trimmed))
        ;; Serialize with size enforcement
        (define json-str (serialize-response response))
        (display json-str (current-output-port))
        (newline (current-output-port))
        (flush-output (current-output-port))
        (worker-loop)])]))

;; ── Entry Point ─────────────────────────────────────────────────

;; When run as a script, start the main loop.
;; When required as a module (for testing), export the functions.

(module+ main
  (worker-loop))

;; ── Provides for testing ────────────────────────────────────────

(provide process-request-line
         worker-loop)
