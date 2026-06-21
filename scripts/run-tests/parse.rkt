#lang racket/base

;; q/scripts/run-tests/parse.rkt — Test output parsing
;;
;; Parses rackunit output, extracts failure lines, normalizes counts.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string)

(provide test-file-result
         test-file-result?
         test-file-result-path
         test-file-result-exit-code
         test-file-result-stdout-bytes
         test-file-result-stderr-bytes
         test-file-result-elapsed-ms
         test-file-result-passed
         test-file-result-failed
         test-file-result-total
         make-test-file-result
         bytes->string*
         parse-raco-output
         normalize-counts
         effective-exit-code
         classify-test-result
         test-result->jsexpr
         extract-failure-lines
         truncate-test-output
         DEFAULT-OUTPUT-CAP)

(define (bytes->string* bs)
  (with-handlers ([exn:fail? (lambda (e) (bytes->string/latin-1 bs))])
    (bytes->string/utf-8 bs)))

(struct test-file-result (path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total)
  #:transparent)

(define (make-test-file-result path
                               exit-code
                               stdout-bytes
                               stderr-bytes
                               elapsed-ms
                               passed
                               failed
                               total)
  (test-file-result path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total))

(define (parse-raco-output stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))
  (define all-matches
    (filter
     values
     (for/list ([line (in-list lines)])
       (regexp-match
        #px"([0-9]+) success\\(es\\) ([0-9]+) failure\\(s\\)(?: ([0-9]+) error\\(s\\))? ([0-9]+) test\\(s\\) run"
        line))))
  (cond
    [(pair? all-matches)
     (define passed (for/sum ([m (in-list all-matches)]) (string->number (cadr m))))
     (define failed
       (for/sum ([m (in-list all-matches)])
                (+ (string->number (caddr m))
                   (let ([errors (list-ref m 3)])
                     (if errors
                         (string->number errors)
                         0)))))
     (values passed failed (+ passed failed))]
    [else
     (define passed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? passed" line))
         (if m
             (string->number (cadr m))
             n)))
     (define failed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? failed" line))
         (if m
             (string->number (cadr m))
             n)))
     (cond
       [(> (+ passed failed) 0) (values passed failed (+ passed failed))]
       [else
        (define result-successes (length (regexp-match* #rx"#<test-success>" output)))
        (define result-failures
          (+ (length (regexp-match* #rx"#<test-failure>" output))
             (length (regexp-match* #rx"#<test-error>" output))))
        (values result-successes result-failures (+ result-successes result-failures))])]))

(define (normalize-counts exit-code passed failed total)
  (values passed failed total))

(define (effective-exit-code exit-code failed)
  (if (and (= exit-code 0) (> failed 0)) 1 exit-code))

(define (result-output-string r)
  (string-downcase (string-append (bytes->string* (test-file-result-stdout-bytes r))
                                  "\n"
                                  (bytes->string* (test-file-result-stderr-bytes r)))))

(define (output-matches-any? output patterns)
  (for/or ([pat (in-list patterns)])
    (regexp-match? pat output)))

(define (classify-test-result r)
  (define exit-code (test-file-result-exit-code r))
  (define failed (test-file-result-failed r))
  (define total (test-file-result-total r))
  (define output (result-output-string r))
  (cond
    [(or (= exit-code 5) (regexp-match? #rx"skipped_by_profile" output)) 'SKIPPED_BY_PROFILE]
    [(and (= exit-code 0) (> total 0)) 'PASS]
    [(and (= exit-code 0) (= total 0)) 'ZERO_PARSED]
    [(= exit-code 2) 'TIMEOUT]
    [(output-matches-any? output (list #rx"user break" #rx"break exception")) 'USER_BREAK]
    [(output-matches-any? output
                          (list #rx"missing environment variable"
                                #rx"environment variable"
                                #rx"api[_-]?key"
                                #rx"display.*not available"
                                #rx"x11"
                                #rx"no such file or directory"))
     'ENVIRONMENT_MISSING]
    [(output-matches-any?
      output
      (list #rx"read-syntax" #rx"syntax error" #rx"module: identifier already defined"))
     'COMPILE_FAILURE]
    [(output-matches-any? output
                          (list #rx"standard-module-name-resolver"
                                #rx"collection not found"
                                #rx"cannot open module file"
                                #rx"cannot find module"
                                #rx"cannot open input file"))
     'MODULE_LOAD_FAILURE]
    [(or (> failed 0)
         (output-matches-any? output
                              (list #rx"failure" #rx"check-[a-z0-9-]+" #rx"actual:" #rx"expected:")))
     'ASSERTION_FAILURE]
    [else 'UNKNOWN_FAILURE]))

(define (test-result->jsexpr r)
  (hasheq 'path
          (let ([p (test-file-result-path r)])
            (if (path? p)
                (path->string p)
                p))
          'category
          (symbol->string (classify-test-result r))
          'exit_code
          (test-file-result-exit-code r)
          'elapsed_ms
          (test-file-result-elapsed-ms r)
          'passed
          (test-file-result-passed r)
          'failed
          (test-file-result-failed r)
          'total
          (test-file-result-total r)
          'output
          (truncate-test-output (result-output-string r) DEFAULT-OUTPUT-CAP)))

(define FAILURE-START #rx"^-+ FAILURE -+$")
;; FIXED (v0.99.39): Changed from #rx to #px mode. The {20,} quantifier
;; is literal in #rx mode but a valid repetition in #px (PCRE) mode.
;; W3 F1 bug — this regex never matched "----..." separator lines before.
(define FAILURE-END #px"^-{20,}$")

(define (extract-failure-lines stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))
  (define in-failure? (box #f))
  (for/list ([line (in-list lines)]
             #:when (cond
                      [(regexp-match? FAILURE-START line)
                       (set-box! in-failure? #t)
                       #t]
                      [(and (unbox in-failure?)
                            (regexp-match? FAILURE-END line)
                            (not (regexp-match? FAILURE-START line)))
                       (set-box! in-failure? #f)
                       #t]
                      [(unbox in-failure?) #t]
                      [else #f]))
    line))

(define DEFAULT-OUTPUT-CAP 65536)

(define (truncate-test-output output max-bytes)
  (define bts (string->bytes/utf-8 output))
  (if (<= (bytes-length bts) max-bytes)
      output
      (let* ([half (quotient max-bytes 2)]
             [head (subbytes bts 0 half)]
             [tail (subbytes bts (- (bytes-length bts) half))]
             [marker (string->bytes/utf-8 (format "\n... truncated (~a bytes, showing ~a + ~a) ...\n"
                                                  (bytes-length bts)
                                                  half
                                                  half))])
        (bytes->string/utf-8 (bytes-append head marker tail)))))
