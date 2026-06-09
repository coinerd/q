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

(define FAILURE-START #rx"^-+ FAILURE -+$")
(define FAILURE-END #rx"^-{20,}$")

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
