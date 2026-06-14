#lang racket/base

;; sandbox/worker-main.rkt — Execution Plane Worker
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

(require racket/match
         json
         "ipc-protocol.rkt"
         "worker-tools.rkt")

;; ── Request Processing ──────────────────────────────────────────

(define (process-request-line line)
  ;; Parse JSON → ipc-request → dispatch → ipc-response
  ;; Returns ipc-response
  ;; M5: Use parameterize for CWD so changes don't leak across requests
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-response #f (format "worker error: ~a" (exn-message e))))])
    (define req-data
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (string->jsexpr line)))
    (define request (and req-data (jsexpr->ipc-request req-data)))
    (cond
      [(not request) (make-error-response #f "malformed request")]
      [else
       (define req-id (ipc-request-request-id request))
       (define tool-name (ipc-request-tool-name request))
       (define arguments (ipc-request-arguments request))
       ;; M5: parameterize restores CWD after dispatch (no leak)
       (define response
         (let ([wd (ipc-request-working-dir request)])
           (if wd
               (parameterize ([current-directory wd])
                 (dispatch-tool tool-name arguments))
               (dispatch-tool tool-name arguments))))
       ;; Stamp the request-id into the response
       (ipc-response req-id
                     (ipc-response-status response)
                     (ipc-response-content response)
                     (ipc-response-details response)
                     (ipc-response-error-message response)
                     (ipc-response-schema-version response))])))

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
        ;; Normalize void content to #f for JSON serialization
        (define clean-response
          (if (void? (ipc-response-content response))
              (ipc-response (ipc-response-request-id response)
                            (ipc-response-status response)
                            #f
                            (ipc-response-details response)
                            (ipc-response-error-message response)
                            (ipc-response-schema-version response))
              response))
        ;; M4: Enforce IPC-MAX-RESPONSE-BYTES — prevent oversized responses from crashing
        (define json-str (jsexpr->string (ipc-response->jsexpr clean-response)))
        (define final-json-str
          (if (> (string-length json-str) IPC-MAX-RESPONSE-BYTES)
              (let ([truncated (ipc-response (ipc-response-request-id clean-response)
                                             'error
                                             (format "response too large (~a bytes, max ~a)"
                                                     (string-length json-str)
                                                     IPC-MAX-RESPONSE-BYTES)
                                             (hasheq 'original-bytes (string-length json-str))
                                             "response exceeded size limit"
                                             (ipc-response-schema-version clean-response))])
                (jsexpr->string (ipc-response->jsexpr truncated)))
              json-str))
        (display final-json-str (current-output-port))
        (newline (current-output-port))
        (flush-output (current-output-port))
        (worker-loop)])]))

;; ── Entry Point ─────────────────────────────────────────────────

;; When run as a script, start the main loop.
;; When required as a module (for testing), export the functions.

(module+ main
  (worker-loop))

;; ── Requires for string-trim ────────────────────────────────────

(require racket/string)

;; ── Provides for testing ────────────────────────────────────────

(provide process-request-line
         worker-loop)
