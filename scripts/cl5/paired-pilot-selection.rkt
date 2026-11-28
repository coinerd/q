#lang racket/base

;; Evaluation-only decision step for the CL5 same-runner paired-measurement pilot.
;; It computes a REAL contract decision on the frozen revision and projects it onto the
;; frozen reduced surface. It runs no tests and changes no production selection.

(require json
         racket/file
         racket/format
         racket/cmdline
         racket/list
         racket/string
         "../../util/sha256.rkt"
         "contract-escalation.rkt")

(provide decision-digest
         project-selection
         compute-selection)

(define (decision-digest decision)
  (define canonical
    (list (hash-ref decision 'verdict)
          (sort (hash-ref decision 'required) string<?)
          (sort (for/list ([e (in-list (hash-ref decision 'escalations))])
                  (list (hash-ref e 'code) (hash-ref e 'module)))
                (lambda (a b) (string<? (car a) (car b))))))
  (sha256-hex (string->bytes/utf-8 (~s canonical))))

(define (project-selection required comprehensive)
  (sort (filter (lambda (f) (member f comprehensive)) required) string<?))

(define (compute-selection root workload)
  (define changed (hash-ref (hash-ref workload 'case) 'changed_files))
  (define start (current-inexact-milliseconds))
  (define decision (evaluate-contract-changes root changed))
  (define overhead-ms (inexact->exact (round (- (current-inexact-milliseconds) start))))
  (define comprehensive (hash-ref workload 'comprehensive_reduced_files))
  (define projected (project-selection (hash-ref decision 'required) comprehensive))
  (unless (equal? projected (hash-ref workload 'selected_projected_files))
    (error 'compute-selection "projected selected surface drifted from the frozen workload"))
  (unless (equal? (decision-digest decision) (hash-ref (hash-ref workload 'case) 'decision_sha256))
    (error 'compute-selection "contract decision digest drifted from the frozen workload"))
  (hasheq 'schema
          "cl5-w3-paired-selection/1"
          'workload_sha256
          (hash-ref workload 'workload_sha256 #f)
          'changed_files
          changed
          'verdict
          (symbol->string (hash-ref decision 'verdict))
          'required
          (sort (hash-ref decision 'required) string<?)
          'escalations
          (for/list ([e (in-list (hash-ref decision 'escalations))])
            (hash 'code (symbol->string (hash-ref e 'code)) 'module (hash-ref e 'module)))
          'decision_sha256
          (decision-digest decision)
          'comprehensive_reduced_files
          comprehensive
          'selected_projected_files
          projected
          'selection_overhead_ms
          overhead-ms))

(module+ main
  (define workload-path #f)
  (define out-path #f)
  (command-line #:program "paired-pilot-selection.rkt"
                #:once-each
                [("--workload") path "frozen paired pilot workload JSON" (set! workload-path path)]
                [("--out") path "selection record JSON" (set! out-path path)]
                #:args ()
                (unless (and workload-path out-path)
                  (raise-user-error 'paired-pilot-selection "--workload and --out are required"))
                (define workload (call-with-input-file workload-path read-json))
                (define record (compute-selection (current-directory) workload))
                (call-with-output-file out-path
                                       (lambda (out)
                                         (write-json record out)
                                         (newline out))
                                       #:exists 'truncate)
                (printf "selection overhead_ms=~a selected=~a verdict=~a\n"
                        (hash-ref record 'selection_overhead_ms)
                        (length (hash-ref record 'selected_projected_files))
                        (hash-ref record 'verdict))))
