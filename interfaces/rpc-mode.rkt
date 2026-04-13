#lang racket/base

(require racket/contract
         racket/string
         racket/math
         json
         racket/port
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt")
;;
;; RPC mode provides a request-response protocol over stdin/stdout
;; using JSONL (one JSON object per line). It follows JSON-RPC 2.0
;; style framing for editor/IDE integration and programmatic access.
;;
;; Request:  {"id": "...", "method": "...", "params": {...}}
;; Response: {"id": "...", "result": ..., "error": null}
;;           {"id": "...", "result": null, "error": {"code": ..., "message": "..."}}
;; Notification: {"jsonrpc": "2.0", "method": "...", "params": {...}}

(provide
 ;; Structs
 (struct-out rpc-request)
 (struct-out rpc-response)
 (struct-out rpc-notification)

 ;; Parsing
 parse-rpc-request

 ;; Serialization
 rpc-response->json
 rpc-notification->json

 ;; Error helper
 rpc-error

 ;; Error codes
 RPC-ERROR-PARSE
 RPC-ERROR-INVALID-REQUEST
 RPC-ERROR-METHOD-NOT-FOUND
 RPC-ERROR-INVALID-PARAMS
 RPC-ERROR-INTERNAL
 RPC-ERROR-HANDSHAKE-REQUIRED

 ;; Handshake
 generate-handshake-token
 rpc-handshake-valid?

 ;; Dispatch
 dispatch-rpc-request

 ;; RPC loop
 run-rpc-loop

 ;; Event forwarding
 start-rpc-event-forwarding!
 stop-rpc-event-forwarding!)

;; ============================================================
;; Structs
;; ============================================================

(struct rpc-request (id method params) #:transparent)
(struct rpc-response (id result error) #:transparent)
(struct rpc-notification (method params) #:transparent)

;; ============================================================
;; Standard error codes (JSON-RPC 2.0 compatible)
;; ============================================================

(define RPC-ERROR-PARSE -32700)
(define RPC-ERROR-INVALID-REQUEST -32600)
(define RPC-ERROR-METHOD-NOT-FOUND -32601)
(define RPC-ERROR-INVALID-PARAMS -32602)
(define RPC-ERROR-INTERNAL -32603)
(define RPC-ERROR-HANDSHAKE-REQUIRED -32001)

;; ============================================================
;; Handshake token (SEC-15)
;; ============================================================

(define (generate-handshake-token)
  ;; Generate a cryptographically secure 128-bit handshake token (SEC-12)
  ;; Falls back to timestamp+random if crypto module unavailable
  (with-handlers ([exn:fail? (λ (_)
                               (format "~a-~a" (exact-truncate (current-inexact-milliseconds))
                                       (random 1000000000)))])
    (define bytes ((dynamic-require 'racket/crypto 'crypto-random-bytes) 16))
    (format "~a" (bytes->hex-string bytes))))

;; Helper: convert bytes to hex string
(define (bytes->hex-string bs)
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (let ([s (number->string b 16)])
             (if (< b 16) (string-append "0" s) s)))))

(define (rpc-handshake-valid? line expected-token)
  ;; Check if line is a valid handshake with the expected token.
  ;; Accepts: {"method":"handshake","params":{"token":"<TOKEN>"}}
  (with-handlers ([exn:fail? (λ (_) #f)])
    (let ([js (read-json (open-input-string (string-trim line)))])
      (and (hash? js)
           (equal? (hash-ref js 'method #f) "handshake")
           (let ([params (hash-ref js 'params #f)])
             (and (hash? params)
                  (equal? (hash-ref params 'token #f) expected-token)))))))

;; ============================================================
;; Error helper
;; ============================================================

(define (rpc-error id code message)
  (rpc-response id #f (hasheq 'code code 'message message)))

;; ============================================================
;; parse-rpc-request : string? -> (or/c rpc-request? #f)
;; ============================================================

(define (parse-rpc-request json-line)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (let* ([trimmed (string-trim json-line)])
      (if (string=? trimmed "")
          #f
          (let ([js (read-json (open-input-string trimmed))])
            (if (not (hash? js))
                #f
                (let ([id (hash-ref js 'id #f)]
                      [method-str (hash-ref js 'method #f)]
                      [raw-params (hash-ref js 'params #f)])
                  (if (not (and id method-str (string? method-str)))
                      #f
                      (let ([params (if (and raw-params (hash? raw-params))
                                        raw-params
                                        (make-immutable-hash))])
                        (rpc-request id
                                     (string->symbol method-str)
                                     params))))))))))

;; ============================================================
;; rpc-response->json : rpc-response? -> string?
;; ============================================================

(define (rpc-response->json resp)
  (let ([h (hasheq 'id (rpc-response-id resp)
                    'result (rpc-response-result resp)
                    'error (rpc-response-error resp))])
    (with-output-to-string (λ () (write-json h)))))

;; ============================================================
;; rpc-notification->json : rpc-notification? -> string?
;; ============================================================

(define (rpc-notification->json notif)
  (let ([h (hasheq 'jsonrpc "2.0"
                    'method (symbol->string (rpc-notification-method notif))
                    'params (rpc-notification-params notif))])
    (with-output-to-string (λ () (write-json h)))))

;; ============================================================
;; dispatch-rpc-request : rpc-request? (hash/c symbol? procedure?)
;;                       -> rpc-response?
;; ============================================================

(define (dispatch-rpc-request req handlers)
  (let* ([method (rpc-request-method req)]
         [id (rpc-request-id req)]
         [handler (hash-ref handlers method #f)])
    (if handler
        (with-handlers ([exn:fail?
                         (λ (e)
                           (rpc-error id RPC-ERROR-INTERNAL (exn-message e)))])
          (let ([result (handler (rpc-request-params req))])
            (rpc-response id result #f)))
        (rpc-error id RPC-ERROR-METHOD-NOT-FOUND
                   (format "Method not found: ~a" method)))))

;; ============================================================
;; Helper: write an RPC response as a JSONL line
;; ============================================================

(define (write-rpc-line out resp)
  (displayln (rpc-response->json resp) out)
  (flush-output out))

;; ============================================================
;; run-rpc-loop : (hash/c symbol? procedure?)
;;                #:input-port input-port?
;;                #:output-port output-port?
;;              -> void?
;; ============================================================

(define (run-rpc-loop handlers
                       #:input-port [in (current-input-port)]
                       #:output-port [out (current-output-port)]
                       #:handshake-token [handshake-token #f])
  ;; SEC-15: If handshake token provided, require it before any commands
  (define authenticated? (box (not handshake-token)))
  ;; Read lines until handshake succeeds (if required)
  (when handshake-token
    (let handshake-loop ()
      (let ([line (read-line in)])
        (unless (eof-object? line)
          (let ([trimmed (string-trim line)])
            (cond
              [(string=? trimmed "") (handshake-loop)]
              [(rpc-handshake-valid? line handshake-token)
               (set-box! authenticated? #t)]
              [else
               (write-rpc-line out (rpc-response #f #f (hasheq 'code RPC-ERROR-HANDSHAKE-REQUIRED
                                                                  'message "Handshake required")))
               ;; Close connection on failed handshake
               (close-input-port in)
               (close-output-port out)]))))))
  ;; Main command loop — only runs if authenticated
  (when (unbox authenticated?)
    (let loop ()
      (let ([line (read-line in)])
        (unless (eof-object? line)
          (let ([trimmed (string-trim line)])
            (if (string=? trimmed "")
                (loop)
                (let ([req (parse-rpc-request line)])
                  (if req
                      (let ([resp (dispatch-rpc-request req handlers)])
                        (write-rpc-line out resp)
                        (unless (eq? (rpc-request-method req) 'shutdown)
                          (loop)))
                      ;; Invalid request — determine error type
                      (let ([valid-json?
                             (with-handlers ([exn:fail? (λ (_) #f)])
                               (read-json (open-input-string trimmed))
                               #t)])
                        (if valid-json?
                            (let* ([parsed (read-json (open-input-string trimmed))]
                                   [id (hash-ref parsed 'id #f)])
                              (write-rpc-line out (rpc-error id RPC-ERROR-INVALID-REQUEST
                                                              "Invalid request")))
                            (write-rpc-line out (rpc-response #f #f (hasheq 'code RPC-ERROR-PARSE
                                                                             'message "Parse error"))))
                        (loop)))))))))))

;; ============================================================
;; Event forwarding
;; ============================================================

;; Convert a runtime event to an RPC notification
(define (event->rpc-notification evt)
  (rpc-notification (string->symbol (event-ev evt))
                    (event->jsexpr evt)))

;; start-rpc-event-forwarding! : event-bus? output-port? -> exact-nonnegative-integer?
(define (start-rpc-event-forwarding! bus output-port)
  (subscribe! bus
              (λ (evt)
                (let ([notif (event->rpc-notification evt)])
                  (displayln (rpc-notification->json notif) output-port)
                  (flush-output output-port)))))

;; stop-rpc-event-forwarding! : event-bus? exact-nonnegative-integer? -> void?
(define (stop-rpc-event-forwarding! bus subscription-id)
  (unsubscribe! bus subscription-id))
