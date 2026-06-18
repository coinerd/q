#!/usr/bin/env racket
#lang racket/base

;; @not-test
;; Mock worker for gateway-ipc tests.
;; Reads newline-delimited JSON on stdin, echoes back responses.
;;
;; Modes (via first arg):
;;   echo    — echo request as ok response (default)
;;   delay:N — wait N seconds then respond
;;   crash   — exit immediately after reading first line
;;   stderr  — write to stderr then respond
;;   slow    — respond slowly (0.5s per request)
;;   real    — M6: use real tool dispatch (bash) for faithful responses

(require json
         racket/match
         racket/string
         (only-in racket/port port->string))

(define mode
  (with-handlers ([exn:fail? (lambda (_) "echo")])
    (vector-ref (current-command-line-arguments) 0)))

(flush-output (current-output-port))

(define (make-ok-response req-id content [details (hasheq)])
  (jsexpr->string (hasheq 'request-id
                          req-id
                          'status
                          "ok"
                          'content
                          content
                          'details
                          details
                          'error-message
                          #f
                          'schema-version
                          1)))

(define (make-err-response req-id msg)
  (jsexpr->string (hasheq 'request-id
                          req-id
                          'status
                          "error"
                          'content
                          #f
                          'details
                          (hasheq)
                          'error-message
                          msg
                          'schema-version
                          1)))

(define (run-bash-command cmd)
  ;; M6: Run bash command and capture output using subprocess
  (define sh-path (find-executable-path "sh"))
  (define-values (proc out-port in-port err-port) (subprocess #f #f #f sh-path "-c" cmd))
  (define output (port->string out-port))
  (subprocess-wait proc)
  (close-input-port out-port)
  (close-output-port in-port)
  (close-input-port err-port)
  output)

(define (process-line line)
  (with-handlers ([exn:fail? (lambda (e)
                               (write-string (make-err-response "error" (exn-message e)))
                               (newline))])
    (define req (string->jsexpr line))
    (define req-id (hash-ref req 'request-id "unknown"))
    (cond
      [(string=? mode "crash") (exit 1)]
      [(string=? mode "stderr")
       (displayln "error: something went wrong on stderr" (current-error-port))
       (write-string (make-ok-response req-id "responded-with-stderr"))
       (newline)]
      [(string-prefix? mode "delay:")
       (define secs (string->number (substring mode 6)))
       (sleep secs)
       (write-string (make-ok-response req-id "delayed-response" (hasheq 'delay-secs secs)))
       (newline)]
      [(string=? mode "slow")
       (sleep 0.5)
       (write-string (make-ok-response req-id "slow-response"))
       (newline)]
      [else
       ;; M6: real dispatch mode runs bash commands faithfully
       (define tool-name (hash-ref req 'tool-name "unknown"))
       (define args (hash-ref req 'arguments (hasheq)))
       (cond
         [(and (string=? mode "real") (equal? tool-name "bash"))
          (define cmd (hash-ref args 'command #f))
          (if cmd
              (let ([out (run-bash-command cmd)])
                (write-string (make-ok-response req-id out))
                (newline))
              (begin
                (write-string (make-err-response req-id "missing command"))
                (newline)))]
         [else
          ;; Default echo mode
          (write-string (make-ok-response req-id tool-name args))
          (newline)])])
    (flush-output)))

(let loop ()
  (define line (read-line))
  (unless (eof-object? line)
    (define trimmed (string-trim line))
    (unless (string=? trimmed "")
      (process-line trimmed))
    (loop)))
