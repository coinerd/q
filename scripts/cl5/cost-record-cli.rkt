#lang racket/base

;; cost-record-cli.rkt — build or verify a CL5 cost record from production-runner
;; artifacts (#9982). Evaluation-only: it summarises artifacts CI already writes
;; and re-derives every aggregate; it never schedules, never executes a test and
;; never decides what "comprehensive" means (see docs/reports/CL5-P2-COST-DESIGN.md).

(require json
         racket/cmdline
         racket/port
         racket/file
         racket/path
         racket/string
         "cost-record.rkt")

(define (read-json-file p)
  (with-handlers ([exn:fail? (lambda (e)
                               (error 'cost-record-cli "cannot read ~a: ~a" p (exn-message e)))])
    (call-with-input-file p (lambda (in) (read-json in)))))

;; A producer spec either names a production runner `run_json` artifact (its own
;; `--json-out` output, exactly as CI writes it) or an `inline` file list for a
;; command that emits no JSON (the smoke workflow step).
(define (producer-from-spec s dir)
  (define name (hash-ref s 'name))
  (define adapter (string->symbol (hash-ref s 'adapter "runner")))
  (define run-json (hash-ref s 'run_json #f))
  (define inline (hash-ref s 'inline #f))
  (define jobs (hash-ref s 'jobs 1))
  (define shard (hash-ref s 'shard "none"))
  (define lane (hash-ref s 'lane #f))
  (define command (hash-ref s 'command ""))
  (define wall (hash-ref s 'wall_s))
  (cond
    [(and run-json inline)
     (error 'cost-record-cli "producer ~a: run_json and inline are exclusive" name)]
    [run-json
     (define raw
       (read-json-file (if (absolute-path? run-json)
                           run-json
                           (build-path dir run-json))))
     (define summary (hash-ref raw 'run_summary))
     (define prep (hash-ref (hash-ref raw 'extra (hash)) 'prepared_environment (hash)))
     (record-producer name
                      adapter
                      lane
                      shard
                      jobs
                      command
                      (for/list ([f (in-list (hash-ref raw 'files '()))])
                        (hash 'path
                              (hash-ref f 'path)
                              'elapsed_ms
                              (hash-ref f 'elapsed_ms)
                              'status
                              (hash-ref f 'status "unknown")))
                      wall
                      (hash-ref prep 'preparation_elapsed_ms #f)
                      (hash-ref summary 'file_count)
                      (hash-ref summary 'pass)
                      (hash-ref summary 'fail)
                      (hash-ref summary 'timeout)
                      (hash-ref summary 'skip)
                      (hash-ref raw 'verdict (symbol->string (hash-ref summary 'verdict 'unknown))))]
    [inline
     (define files
       (for/list ([f (in-list inline)])
         (hash 'path
               (hash-ref f 'path)
               'elapsed_ms
               (hash-ref f 'elapsed_ms)
               'status
               (hash-ref f 'status "pass"))))
     (define timeouts
       (for/sum ([f (in-list files)]) (if (equal? (hash-ref f 'status) "timeout") 1 0)))
     (record-producer name
                      adapter
                      lane
                      shard
                      jobs
                      command
                      files
                      wall
                      #f
                      (length files)
                      (- (length files) timeouts)
                      0
                      timeouts
                      0
                      (hash-ref s 'verdict "pass"))]
    [else (error 'cost-record-cli "producer ~a: needs run_json or inline" name)]))

(define spec-path (make-parameter #f))
(define out-path (make-parameter #f))
(define verify-path (make-parameter #f))

(command-line
 #:program "cost-record.rkt"
 #:once-each [("--from-spec") p "Build a record from a measurement spec" (spec-path p)]
 [("--out") p "Write the record here" (out-path p)]
 [("--verify") p "Verify an existing record" (verify-path p)]
 #:args ()
 (cond
   [(verify-path)
    (define summary
      (with-handlers ([exn:fail? (lambda (e)
                                   (eprintf "~a~n" (exn-message e))
                                   (exit 1))])
        (verify-record (read-json-file (verify-path)))))
    (printf "record verified: ~a producers, comprehensive ~a s (~a), ~a file slots, ~a timeouts~n"
            (hash-ref summary 'producers)
            (hash-ref summary 'comprehensive-wall-s)
            (hash-ref summary 'status)
            (hash-ref summary 'files)
            (hash-ref summary 'timeouts))]
   [(spec-path)
    (unless (out-path)
      (error 'cost-record-cli "--from-spec needs --out"))
    (define spec (read-json-file (spec-path)))
    (define dir (path-only (path->complete-path (spec-path))))
    (define producers
      (for/list ([s (in-list (hash-ref spec 'producers))])
        (producer-from-spec s dir)))
    (define rec
      (build-record #:revision (hash-ref spec 'revision)
                    #:tree (hash-ref spec 'tree)
                    #:host (hash-ref spec 'host)
                    #:cache (hash-ref spec 'cache)
                    #:producers producers
                    #:notes (hash-ref spec 'notes '())))
    ;; Round-trip through JSON before writing: the file that lands on disk is the
    ;; file the verifier accepts, or nothing is written at all.
    (verify-record (with-input-from-string (string-append (jsexpr->string rec)) read-json))
    (call-with-output-file (out-path)
                           #:exists 'replace
                           (lambda (o)
                             (write-json (record->jsexpr rec) o)
                             (newline o)))
    (printf "wrote ~a~n" (out-path))]
   [else
    (eprintf "usage: cost-record-cli.rkt --from-spec SPEC --out REC | --verify REC~n")
    (exit 1)]))
