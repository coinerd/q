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

(provide record-gate-evidence!
         validate-gate-evidence!
         strict-sha-valid?)

;; F-15 strict validation: full 40-hex-char SHA required
(define (strict-sha-valid? sha)
  (and (string? sha) (= (string-length sha) 40) (regexp-match? #px"^[0-9a-f]{40}$" sha)))

;; F-15: Validate recorded evidence hash against strict requirements.
;; Rejects unknown/empty version, short/missing SHA, stale/future timestamp,
;; non-zero failed/timed-out counts, zero test count.
(define (validate-gate-evidence! evidence-hash)
  (define ver (hash-ref evidence-hash 'version #f))
  (define sha (hash-ref evidence-hash 'git-sha #f))
  (define pass-c (hash-ref evidence-hash 'passed 0))
  (define fail-c (hash-ref evidence-hash 'failed -1))
  (define timeout-c (hash-ref evidence-hash 'timed-out -1))
  (define parsed (hash-ref evidence-hash 'parsed-test-count 0))
  (define ts (hash-ref evidence-hash 'timestamp #f))
  (define now (current-seconds))
  (define max-age 7200) ;; 2 hours
  (define max-future 300) ;; 5 minutes clock skew tolerance
  (cond
    [(or (not ver) (equal? ver "unknown"))
     (raise-user-error 'validate-gate-evidence! "version is unknown or missing")]
    [(or (not sha) (equal? sha "unknown"))
     (raise-user-error 'validate-gate-evidence! "git SHA is unknown")]
    [(not (strict-sha-valid? sha))
     (raise-user-error 'validate-gate-evidence! "SHA must be full 40 hex chars; got ~a" sha)]
    [(not (and (integer? parsed) (positive? parsed)))
     (raise-user-error 'validate-gate-evidence! "parsed test count must be positive; got ~a" parsed)]
    [(and (integer? fail-c) (positive? fail-c))
     (raise-user-error 'validate-gate-evidence! "evidence has ~a failure(s)" fail-c)]
    [(and (integer? timeout-c) (positive? timeout-c))
     (raise-user-error 'validate-gate-evidence! "evidence has ~a timeout(s)" timeout-c)]
    [(and ts (integer? ts) (>= (- now ts) max-age))
     (raise-user-error 'validate-gate-evidence! "evidence is stale (~a seconds old)" (- now ts))]
    [(and ts (integer? ts) (>= (- ts now) max-future))
     (raise-user-error 'validate-gate-evidence!
                       "evidence has future timestamp (~a seconds ahead)"
                       (- ts now))]
    [else (void)]))

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
  ;; F-15: Validate recorded evidence hash before writing
  (validate-gate-evidence! evidence-hash)
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
