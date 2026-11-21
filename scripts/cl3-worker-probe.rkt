#lang racket/base

;; scripts/cl3-worker-probe.rkt — CL3 evaluation-only child worker.
;;
;; This is NOT a q production tool. It is a bounded prototype used to measure
;; whether a persistent Racket evaluator is worth proposing in #9911. It runs
;; in a separate OS process; the namespace/custodian inside it manages state
;; only and is never treated as a security boundary.
;;
;; JSON-lines protocol (stdin -> stdout):
;;   {"op":"eval","code":"(+ 20 22)"}
;;   {"op":"load","path":"tests/..."}     {"op":"reload","path":"..."}
;;   {"op":"reset"}                       {"op":"stop"}
;; Responses always include op, status, duration-ms and bounded diagnostics.
;;
;; Resource boundary: output and rendered values are accumulated through
;; fixed-capacity ports, so the worker never builds an unbounded diagnostic
;; string. This is NOT a sandbox: evaluated code still shares the process,
;; threads and OS resources. Worker replacement, not namespace reset, is the
;; stronger clean-process boundary.

(require racket/port
         racket/string
         racket/rerequire
         json)

(provide max-diagnostic-chars
         max-value-chars
         bounded
         make-bounded-collector
         render-value
         response
         worker-loop)

(define max-diagnostic-chars 4096)
(define max-value-chars 512)

(define (bounded text limit)
  (cond
    [(not (string? text)) ""]
    [(<= (string-length text) limit) (cons text #f)]
    [else (cons (substring text 0 limit) #t)]))

;; A custom output port stops storing bytes at `limit` but keeps consuming the
;; producer's bytes, so a large `display` cannot inflate the returned response
;; or force the writer into an unbounded retry loop.
(define (make-bounded-collector limit)
  (define chunks '())
  (define stored-bytes 0)
  (define discarded-bytes 0)
  (define (write-out bstr start end non-block? enable-break?)
    (define available (- end start))
    (define take (min available (max 0 (- limit stored-bytes))))
    (when (positive? take)
      (set! chunks (cons (subbytes bstr start (+ start take)) chunks))
      (set! stored-bytes (+ stored-bytes take)))
    (when (< take available)
      (set! discarded-bytes (+ discarded-bytes (- available take))))
    available)
  (define port (make-output-port 'bounded-collector always-evt write-out void))
  (define (collect)
    (define data (apply bytes-append (reverse chunks)))
    (hash 'text
          (bytes->string/utf-8 data #\uFFFD)
          'truncated?
          (positive? discarded-bytes)
          'stored-bytes
          stored-bytes
          'discarded-bytes
          discarded-bytes))
  (values port collect))

(define (collector->bounded collect limit)
  (define result (collect))
  (cons (hash-ref result 'text)
        (or (hash-ref result 'truncated?) (> (hash-ref result 'stored-bytes) limit))))

(define (render-value value)
  (define-values (port collect) (make-bounded-collector max-value-chars))
  (with-handlers ([exn:fail? (lambda (e)
                               (bounded (format "#<unprintable: ~a>"
                                                (car (bounded (exn-message e) 128)))
                                        max-value-chars))])
    (write value port)
    (flush-output port)
    (collector->bounded collect max-value-chars)))

(define (elapsed-ms start)
  (/ (round (* 1000.0 (- (current-inexact-milliseconds) start))) 1000.0))

(define (make-namespace)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base))
  ns)

(define current-ns (make-namespace))

(define (module-path-for path)
  (path->complete-path (if (absolute-path? (string->path path))
                           (string->path path)
                           (build-path (current-directory) (string->path path)))))

(define (eval-code code)
  (define-values (out-port collect-out) (make-bounded-collector max-diagnostic-chars))
  (define-values (err-port collect-err) (make-bounded-collector max-diagnostic-chars))
  (define values '())
  (define error-data #f)
  (define start (current-inexact-milliseconds))
  (with-handlers ([exn:fail? (lambda (e)
                               (set! error-data
                                     (hash 'message
                                           (car (bounded (exn-message e) max-diagnostic-chars))
                                           'kind
                                           "exn:fail")))])
    (define in (open-input-string code))
    (let loop ()
      (define datum (read in))
      (unless (eof-object? datum)
        (define value
          (parameterize ([current-namespace current-ns]
                         [current-output-port out-port]
                         [current-error-port err-port])
            (eval datum current-ns)))
        (define rendered (render-value value))
        (set! values (cons (hash 'text (car rendered) 'truncated? (cdr rendered)) values))
        (loop))))
  (define stdout (collect-out))
  (define stderr (collect-err))
  (hash 'status
        (if error-data "error" "ok")
        'values
        (reverse values)
        'stdout
        (hash-ref stdout 'text)
        'stderr
        (hash-ref stderr 'text)
        'truncated?
        (or (hash-ref stdout 'truncated?) (hash-ref stderr 'truncated?))
        'bounded-output?
        #t
        'error
        error-data
        'duration-ms
        (elapsed-ms start)))

(define (hash-merge a b)
  (for/fold ([result a]) ([(k v) (in-hash b)])
    (hash-set result k v)))

(define (with-module-op op path thunk)
  (define start (current-inexact-milliseconds))
  (define result
    (with-handlers ([exn:fail? (lambda (e)
                                 (hash 'status
                                       "error"
                                       'error
                                       (hash 'message
                                             (car (bounded (exn-message e) max-diagnostic-chars))
                                             'kind
                                             "exn:fail")))])
      (parameterize ([current-namespace current-ns])
        (thunk (module-path-for path)))
      (hash 'status "ok")))
  (hash-merge result
              (hash 'op
                    op
                    'path
                    path
                    'note
                    (if (equal? op "reload")
                        "module instances refreshed; pre-existing closures and values may be stale"
                        "module loaded into the current namespace")
                    'duration-ms
                    (elapsed-ms start))))

(define (response req)
  (define op (hash-ref req 'op #f))
  (define start (current-inexact-milliseconds))
  (define body
    (case op
      [("eval") (hash-set (eval-code (hash-ref req 'code "")) 'op "eval")]
      [("load") (with-module-op "load" (hash-ref req 'path "") dynamic-require*)]
      [("reload") (with-module-op "reload" (hash-ref req 'path "") reload*)]
      [("reset")
       (set! current-ns (make-namespace))
       (hash 'op
             "reset"
             'status
             "ok"
             'state
             "clean-interactive-namespace"
             'note
             (string-append "new namespace: no prior interactive definitions are retained; "
                            "process-global state, threads and OS resources are NOT reset; "
                            "use worker replacement for a clean-process boundary"))]
      [("stop") (hash 'op "stop" 'status "ok")]
      [else
       (hash 'op
             (or op "unknown")
             'status
             "error"
             'error
             (hash 'message "unknown operation" 'kind "protocol"))]))
  (hash-merge (hash 'duration-ms (elapsed-ms start)) body))

(define (dynamic-require* path)
  (namespace-require (list 'file (path->string path))))
(define (reload* path)
  (define spec (list 'file (path->string path)))
  (dynamic-rerequire spec)
  ;; Re-require after rerequire so NEW lookups see fresh module instances;
  ;; values captured before reload remain stale by design.
  (namespace-require spec))

(define (worker-loop)
  (let loop ()
    (define line (read-line (current-input-port)))
    (unless (eof-object? line)
      (define req
        (with-handlers ([exn:fail? (lambda (_) (hash 'op "unknown"))])
          (string->jsexpr line)))
      (define result (response req))
      (write-json result)
      (newline)
      (flush-output)
      (unless (equal? (hash-ref req 'op #f) "stop")
        (loop)))))

(module+ main
  (worker-loop))
