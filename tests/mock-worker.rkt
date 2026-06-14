#!/usr/bin/env racket
#lang racket/base

;; Mock worker for gateway-ipc tests.
;; Reads newline-delimited JSON on stdin, echoes back responses.
;;
;; Modes (via first arg):
;;   echo    — echo request as ok response (default)
;;   delay:N — wait N seconds then respond
;;   crash   — exit immediately after reading first line
;;   stderr  — write to stderr then respond
;;   slow    — respond slowly (0.5s per request)

(require json
         racket/match
         racket/string)

(define mode
  (with-handlers ([exn:fail? (lambda (_) "echo")])
    (vector-ref (current-command-line-arguments) 0)))

(flush-output (current-output-port))

(define (process-line line)
  (with-handlers ([exn:fail? (lambda (e)
                               (write-string (jsexpr->string (hasheq 'request-id
                                                                     "error"
                                                                     'status
                                                                     "error"
                                                                     'content
                                                                     #f
                                                                     'details
                                                                     (hasheq)
                                                                     'error-message
                                                                     (exn-message e)
                                                                     'schema-version
                                                                     1)))
                               (newline))])
    (define req (string->jsexpr line))
    (define req-id (hash-ref req 'request-id "unknown"))
    (cond
      [(string=? mode "crash") (exit 1)]
      [(string=? mode "stderr")
       (displayln "error: something went wrong on stderr" (current-error-port))
       (write-string (jsexpr->string (hasheq 'request-id
                                             req-id
                                             'status
                                             "ok"
                                             'content
                                             "responded-with-stderr"
                                             'details
                                             (hasheq)
                                             'error-message
                                             #f
                                             'schema-version
                                             1)))
       (newline)]
      [(string-prefix? mode "delay:")
       (define secs (string->number (substring mode 6)))
       (sleep secs)
       (write-string (jsexpr->string (hasheq 'request-id
                                             req-id
                                             'status
                                             "ok"
                                             'content
                                             "delayed-response"
                                             'details
                                             (hasheq 'delay-secs secs)
                                             'error-message
                                             #f
                                             'schema-version
                                             1)))
       (newline)]
      [(string=? mode "slow")
       (sleep 0.5)
       (write-string (jsexpr->string (hasheq 'request-id
                                             req-id
                                             'status
                                             "ok"
                                             'content
                                             "slow-response"
                                             'details
                                             (hasheq)
                                             'error-message
                                             #f
                                             'schema-version
                                             1)))
       (newline)]
      [else
       ;; Default echo mode
       (write-string (jsexpr->string (hasheq 'request-id
                                             req-id
                                             'status
                                             "ok"
                                             'content
                                             (hash-ref req 'tool-name "unknown")
                                             'details
                                             (hash-ref req 'arguments (hasheq))
                                             'error-message
                                             #f
                                             'schema-version
                                             1)))
       (newline)])
    (flush-output)))

(let loop ()
  (define line (read-line))
  (unless (eof-object? line)
    (define trimmed (string-trim line))
    (unless (string=? trimmed "")
      (process-line trimmed))
    (loop)))
