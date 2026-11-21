#lang racket/base

;; scripts/cl3-eval-probe.rkt — CL3 evaluation-only driver for the child worker.
;; Never imported by q's runtime/tools: it exists only to measure #9911 tradeoffs.
;; Requests are bounded by a wall-clock timeout; timeouts/crashes always kill
;; and reap the child before returning.

(require racket/format
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         json)

(provide worker-session?
         worker-session-proc
         start-worker!
         request!
         stop-worker!
         kill-worker!)

(define-runtime-path worker-script "cl3-worker-probe.rkt")

(struct worker-session (proc out in err) #:transparent)

(define (start-worker! #:directory [dir (current-directory)])
  (define-values (proc out in err)
    (parameterize ([current-directory dir])
      (subprocess #f
                  #f
                  #f
                  (or (find-executable-path "racket") (error 'cl3 "racket not found"))
                  (path->string worker-script))))
  (worker-session proc out in err))

(define (close-port* p)
  (with-handlers ([exn:fail? void])
    (when p
      (close-output-port p))))

(define (close-input-port* p)
  (with-handlers ([exn:fail? void])
    (when p
      (close-input-port p))))

(define (kill-worker! session)
  (define proc (worker-session-proc session))
  (with-handlers ([exn:fail? void])
    (subprocess-kill proc #t))
  (with-handlers ([exn:fail? void])
    (subprocess-wait proc))
  (close-port* (worker-session-in session))
  (close-input-port* (worker-session-out session))
  (close-input-port* (worker-session-err session))
  (hash 'status
        "terminated"
        'exit-status
        (with-handlers ([exn:fail? (lambda (_) "unknown")])
          (subprocess-status proc))))

(define (request! session request #:timeout [timeout-seconds 5])
  (define start (current-inexact-milliseconds))
  (define result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (hash 'status "driver-error" 'error (hash 'message (exn-message e))))])
      (write-json request (worker-session-in session))
      (newline (worker-session-in session))
      (flush-output (worker-session-in session))
      (define ready? (sync/timeout timeout-seconds (worker-session-out session)))
      (cond
        [(not ready?)
         (kill-worker! session)
         (hash 'status
               "timeout"
               'error
               (hash 'message (format "worker exceeded ~a seconds" timeout-seconds))
               'terminated?
               #t)]
        [else
         (define line (read-line (worker-session-out session)))
         (cond
           [(eof-object? line)
            (kill-worker! session)
            (hash 'status
                  "crash"
                  'error
                  (hash 'message "worker exited before responding")
                  'terminated?
                  #t)]
           [else
            (with-handlers ([exn:fail? (lambda (e)
                                         (hash 'status
                                               "protocol-error"
                                               'error
                                               (hash 'message (exn-message e))
                                               'raw
                                               line))])
              (string->jsexpr line))])])))
  (hash-set result 'wall-ms (inexact->exact (round (- (current-inexact-milliseconds) start)))))

(define (stop-worker! session)
  (define response
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (hash 'status "driver-error" 'error (hash 'message (exn-message e))))])
      (request! session (hash 'op "stop") #:timeout 3)))
  (define cleanup (kill-worker! session))
  (hash 'response response 'cleanup cleanup))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
    [(null? args) (displayln "usage: cl3-eval-probe <eval|load|reload|reset> [arg ...]")]
    [else
     (define session (start-worker!))
     (define op (string->symbol (car args)))
     (define request
       (case op
         [(eval) (hash 'op "eval" 'code (string-join (cdr args) " "))]
         [(load reload) (hash 'op (symbol->string op) 'path (cadr args))]
         [(reset) (hash 'op "reset")]
         [else (hash 'op "unknown")]))
     (define response (request! session request))
     (write-json response)
     (newline)
     (stop-worker! session)]))
