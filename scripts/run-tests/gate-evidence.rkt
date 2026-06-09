#lang racket/base

;; q/scripts/run-tests/gate-evidence.rkt — Gate evidence recording
;;
;; Records test gate evidence as JSON files for CI verification.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string
         racket/port
         racket/system
         (only-in "parse.rkt"
                  test-file-result-failed
                  test-file-result-total
                  test-file-result-passed
                  test-file-result-exit-code)
         (only-in "classify.rkt" base-dir))

(provide record-gate-evidence!)

(define (get-git-sha)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define-values (sp out in err)
      (subprocess #f #f #f (find-executable-path "git") "rev-parse" "HEAD"))
    (close-output-port in)
    (define sha (string-trim (port->string out)))
    (close-input-port out)
    (close-input-port err)
    (subprocess-wait sp)
    (if (string=? sha "") "unknown" sha)))

(define (write-json h out)
  (display "{" out)
  (define pairs
    (list (cons "version" (format "~s" (hash-ref h 'version)))
          (cons "git_sha" (format "~s" (hash-ref h 'git-sha)))
          (cons "suite" (format "~s" (hash-ref h 'suite)))
          (cons "args"
                (format "[~a]"
                        (string-join (for/list ([a (hash-ref h 'args)])
                                       (format "~s" a))
                                     ", ")))
          (cons "jobs" (format "~a" (hash-ref h 'jobs)))
          (cons "timeout"
                (let ([t (hash-ref h 'timeout)])
                  (if (string? t)
                      (format "~s" t)
                      (format "~a" t))))
          (cons "repeat" (format "~a" (hash-ref h 'repeat)))
          (cons "selected_file_count" (format "~a" (hash-ref h 'selected-file-count)))
          (cons "parsed_test_count" (format "~a" (hash-ref h 'parsed-test-count)))
          (cons "passed" (format "~a" (hash-ref h 'passed)))
          (cons "failed" (format "~a" (hash-ref h 'failed)))
          (cons "timed_out" (format "~a" (hash-ref h 'timed-out)))
          (cons "inventory_hash" (format "~s" (hash-ref h 'inventory-hash)))
          (cons "timestamp" (format "~a" (hash-ref h 'timestamp)))))
  (for ([p (in-list pairs)]
        [i (in-naturals)])
    (when (> i 0)
      (display ", " out))
    (display (format "\"~a\": ~a" (car p) (cdr p)) out))
  (displayln "}" out))

(define (record-gate-evidence! suite-label
                               #:results [results '()]
                               #:args [args '()]
                               #:jobs [jobs 1]
                               #:timeout [timeout #f]
                               #:repeat [repeat 1]
                               #:file-count [file-count 0]
                               #:inventory-hash [inv-hash "n/a"])
  (unless (pair? results)
    (raise-user-error 'run-tests "cannot record gate evidence: no results"))
  (define total-parsed (for/sum ([r (in-list results)]) (test-file-result-total r)))
  (when (zero? total-parsed)
    (raise-user-error 'run-tests "cannot record gate evidence: zero parsed tests"))
  (define any-fail?
    (for/or ([r (in-list results)])
      (not (zero? (test-file-result-failed r)))))
  (when any-fail?
    (raise-user-error 'run-tests "cannot record gate evidence: failures present"))
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (unless (directory-exists? evid-dir)
    (make-directory evid-dir))
  (define ver
    (let ([m (regexp-match #rx"define q-version \"([^\"]+)\""
                           (file->string (build-path base-dir "util/version.rkt")))])
      (or (and m (cadr m)) "unknown")))
  (define sha (get-git-sha))
  (define pass-count (for/sum ([r (in-list results)]) (test-file-result-passed r)))
  (define fail-count (for/sum ([r (in-list results)]) (test-file-result-failed r)))
  (define timeout-count (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define evidence-file (build-path evid-dir (format "~a.json" suite-label)))
  (define evidence-hash
    (hash 'version
          ver
          'git-sha
          sha
          'suite
          suite-label
          'args
          (map (lambda (a)
                 (if (symbol? a)
                     (symbol->string a)
                     a))
               args)
          'jobs
          jobs
          'timeout
          (or timeout "none")
          'repeat
          repeat
          'selected-file-count
          file-count
          'parsed-test-count
          total-parsed
          'passed
          pass-count
          'failed
          fail-count
          'timed-out
          timeout-count
          'inventory-hash
          inv-hash
          'timestamp
          (current-seconds)))
  (call-with-output-file evidence-file
                         (lambda (out)
                           (write-json evidence-hash out)
                           (newline out))
                         #:exists 'truncate)
  (printf ";; gate evidence v2 recorded: ~a (v~a) ~a files, ~a tests~n"
          suite-label
          ver
          file-count
          total-parsed))

(require racket/file
         racket/list)
