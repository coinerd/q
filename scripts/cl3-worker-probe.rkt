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

(require racket/port
         racket/string
         racket/rerequire
         json)

(define max-diagnostic-chars 4096)
(define max-value-chars 512)

(define (bounded text limit)
  (cond
    [(not (string? text)) ""]
    [(<= (string-length text) limit) (cons text #f)]
    [else (cons (substring text 0 limit) #t)]))

(define (render-value value)
  (define text
    (with-handlers ([exn:fail? (lambda (e) (format "#<unprintable: ~a>" (exn-message e)))])
      (format "~s" value)))
  (bounded text max-value-chars))

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
  (define out (open-output-string))
  (define err (open-output-string))
  (define values '())
  (define error-data #f)
  (define start (current-inexact-milliseconds))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (set! error-data
                (hash 'message (car (bounded (exn-message e) max-diagnostic-chars)) 'kind "exn:fail")))])
    (define in (open-input-string code))
    (let loop ()
      (define datum (read in))
      (unless (eof-object? datum)
        (define value
          (parameterize ([current-namespace current-ns]
                         [current-output-port out]
                         [current-error-port err])
            (eval datum current-ns)))
        (define rendered (render-value value))
        (set! values (cons (hash 'text (car rendered) 'truncated? (cdr rendered)) values))
        (loop))))
  (define stdout (bounded (get-output-string out) max-diagnostic-chars))
  (define stderr (bounded (get-output-string err) max-diagnostic-chars))
  (hash 'status
        (if error-data "error" "ok")
        'values
        (reverse values)
        'stdout
        (car stdout)
        'stderr
        (car stderr)
        'truncated?
        (or (cdr stdout) (cdr stderr))
        'error
        error-data
        'duration-ms
        (inexact->exact (round (- (current-inexact-milliseconds) start)))))

(define (with-module-op op path thunk)
  (define start (current-inexact-milliseconds))
  (define result
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (hash 'status "error" 'error (hash 'message (car (bounded (exn-message e) max-diagnostic-chars)) 'kind "exn:fail")))])
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
                    (inexact->exact (round (- (current-inexact-milliseconds) start))))))

(define (hash-merge a b)
  (for/fold ([result a]) ([(k v) (in-hash b)])
    (hash-set result k v)))

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
             "clean"
             'note
             "new namespace: no prior definitions or module instances are retained")]
      [("stop") (hash 'op "stop" 'status "ok")]
      [else
       (hash 'op
             (or op "unknown")
             'status
             "error"
             'error
             (hash 'message "unknown operation" 'kind "protocol"))]))
  (hash-merge (hash 'duration-ms (inexact->exact (round (- (current-inexact-milliseconds) start))))
              body))

(define (dynamic-require* path)
  (namespace-require (list 'file (path->string path))))
(define (reload* path)
  (define spec (list 'file (path->string path)))
  (dynamic-rerequire spec)
  ;; Re-require after rerequire so NEW lookups see fresh module instances;
  ;; values captured before reload remain stale by design.
  (namespace-require spec))

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
      (loop))))
