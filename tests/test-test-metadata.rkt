#lang racket

;; BOUNDARY: unit
;; Tests for test-metadata.rkt metadata parser (v0.83.2 W0)

(require rackunit
         racket/file
         racket/port
         racket/runtime-path)

(define-runtime-path meta-path "../scripts/test-metadata.rkt")

(define meta-loaded? (box #f))
(define meta-cache (make-hash))

(define (meta-ref sym)
  (unless (unbox meta-loaded?)
    (dynamic-require meta-path #f)
    (set-box! meta-loaded? #t))
  (hash-ref! meta-cache sym (lambda () (dynamic-require meta-path sym))))

;; ---------------------------------------------------------------------------
;; parse-test-metadata — synthetic files
;; ---------------------------------------------------------------------------

(define (with-temp-test-file content proc)
  (define tmp (make-temporary-file "test-meta-~a.rkt"))
  (call-with-output-file tmp (lambda (out) (display content out)) #:exists 'truncate)
  (define result (proc tmp))
  (delete-file tmp)
  result)

(test-case "parse-test-metadata: no annotations returns all-#f"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n(require rackunit)\n(check-true #t)\n"
                       (lambda (f)
                         (define m (parse f))
                         (check-false ((meta-ref 'metadata-suite) m))
                         (check-false ((meta-ref 'metadata-boundary) m))
                         (check-false ((meta-ref 'metadata-speed) m))
                         (check-false ((meta-ref 'metadata-timeout) m))
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: parses @suite annotation"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @suite runtime\n(require rackunit)\n"
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "runtime")
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: parses all annotations"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file (string-append "#lang racket\n"
                                      ";; @suite tui\n"
                                      ";; @boundary integration\n"
                                      ";; @speed slow\n"
                                      ";; @mutates cwd\n"
                                      ";; @isolation temp-dir\n"
                                      ";; @timeout 30\n"
                                      "(require rackunit)\n")
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "tui")
                         (check-equal? ((meta-ref 'metadata-boundary) m) "integration")
                         (check-equal? ((meta-ref 'metadata-speed) m) "slow")
                         (check-equal? ((meta-ref 'metadata-mutates) m) "cwd")
                         (check-equal? ((meta-ref 'metadata-isolation) m) "temp-dir")
                         (check-equal? ((meta-ref 'metadata-timeout) m) 30)
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: warns on invalid @suite value"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @suite invalid-suite\n(require rackunit)\n"
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "invalid-suite")
                         (define warnings ((meta-ref 'metadata-warnings) m))
                         (check-not-false (ormap (lambda (w) (regexp-match? #rx"invalid value" w))
                                                 warnings)))))

(test-case "parse-test-metadata: warns on invalid @timeout value"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @timeout abc\n(require rackunit)\n"
                       (lambda (f)
                         (define m (parse f))
                         (define warnings ((meta-ref 'metadata-warnings) m))
                         (check-not-false (ormap (lambda (w) (regexp-match? #rx"invalid value" w))
                                                 warnings)))))

(test-case "parse-test-metadata: handles missing file gracefully"
  (define parse (meta-ref 'parse-test-metadata))
  (define m (parse "/nonexistent/path/test.rkt"))
  (check-false ((meta-ref 'metadata-suite) m)))

(test-case "parse-test-metadata: only scans first 30 lines"
  (define parse (meta-ref 'parse-test-metadata))
  ;; Generate 40 lines, annotation on line 35 — should NOT be picked up
  (define content
    (string-append "#lang racket\n"
                   (apply string-append
                          (for/list ([i (in-range 33)])
                            (format ";; line ~a\n" i)))
                   ";; @suite security\n"
                   "(require rackunit)\n"))
  (with-temp-test-file content
                       (lambda (f)
                         (define m (parse f))
                         (check-false ((meta-ref 'metadata-suite) m)))))

;; ---------------------------------------------------------------------------
;; scan-files-metadata
;; ---------------------------------------------------------------------------

(test-case "scan-files-metadata: returns metadata per file"
  (define scan (meta-ref 'scan-files-metadata))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp1 (make-temporary-file "scan-~a.rkt"))
  (define tmp2 (make-temporary-file "scan-~a.rkt"))
  (call-with-output-file tmp1 (lambda (out) (display ";; @suite fast\n" out)) #:exists 'truncate)
  (call-with-output-file tmp2 (lambda (out) (display "#lang racket\n" out)) #:exists 'truncate)
  (define results (scan (list tmp1 tmp2)))
  (check-equal? (length results) 2)
  (check-equal? ((meta-ref 'metadata-file) (first results)) tmp1)
  (delete-file tmp1)
  (delete-file tmp2))

;; ---------------------------------------------------------------------------
;; metadata-report
;; ---------------------------------------------------------------------------

(test-case "metadata-report: outputs scan summary"
  (define report (meta-ref 'metadata-report))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp (make-temporary-file "report-~a.rkt"))
  (call-with-output-file tmp
                         (lambda (out) (display ";; @suite all\n;; @boundary unit\n" out))
                         #:exists 'truncate)
  (define m (parse tmp))
  (define output (with-output-to-string (lambda () (report (list m)))))
  (check-not-false (regexp-match? #rx"1 files scanned, 1 tagged, 0 untagged" output))
  (delete-file tmp))

(test-case "metadata-report: reports untagged count"
  (define report (meta-ref 'metadata-report))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp (make-temporary-file "report-~a.rkt"))
  (call-with-output-file tmp (lambda (out) (display "#lang racket\n" out)) #:exists 'truncate)
  (define m (parse tmp))
  (define output (with-output-to-string (lambda () (report (list m)))))
  (check-not-false (regexp-match? #rx"0 tagged, 1 untagged" output))
  (delete-file tmp))
