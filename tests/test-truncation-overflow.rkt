#lang racket

;;; tests/test-truncation-overflow.rkt — tests for G5.2 tool output truncation
;;; with temp-file overflow

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../util/truncation.rkt")

;; Helper: create a fresh temporary directory for overflow tests
(define (make-overflow-test-dir)
  (define d (make-temporary-file "q-overflow-test-~a" #:base-dir (find-system-path 'temp-dir)))
  (delete-file d)
  (make-directory d)
  d)

;; Run body with a fresh overflow dir; clean up afterward
(define-syntax-rule (with-test-dir dir-expr body ...)
  (let* ([d (make-overflow-test-dir)]
         [dir-expr d])
    (begin0 (parameterize ([output-overflow-dir d])
              body ...)
      (delete-directory/files d #:must-exist? #f))))

(define (make-lines n)
  (string-join (for/list ([i (in-range n)])
                 (format "Line ~a" i))
               "\n"))

(define (make-big-bytes n)
  (make-string n #\x))

;; =====================================================================
;; Test cases
;; =====================================================================

(test-case "small output passes through without temp file"
  (with-test-dir dir
                 (define result (truncate-output-with-overflow "Hello, world!"))
                 (check-equal? result "Hello, world!")
                 ;; No overflow files created
                 (check-equal? (directory-list dir) '())))

(test-case "large output creates temp file with correct content"
  (with-test-dir dir
                 (define big (make-lines (+ MAX-OUTPUT-LINES 500)))
                 (define result (truncate-output-with-overflow big))
                 ;; Result should mention the overflow file
                 (check-true (string-contains? result "[Full output saved to"))
                 ;; There should be exactly one overflow file
                 (define files (directory-list dir))
                 (check-equal? (length files) 1)
                 ;; The overflow file should contain the full output
                 (define overflow-path (build-path dir (car files)))
                 (define saved-content (file->string overflow-path))
                 (check-equal? saved-content big)))

(test-case "temp file path appears in notice"
  (with-test-dir dir
                 (define big (make-big-bytes (+ MAX-OUTPUT-BYTES 1000)))
                 (define result (truncate-output-with-overflow big "tool-run"))
                 ;; The notice should contain the directory path
                 (check-true (string-contains? result (path->string dir)))
                 ;; The notice should mention total bytes
                 (check-true (string-contains? result "total bytes"))
                 ;; The file basename should start with "tool-run-"
                 (define files (directory-list dir))
                 (check-true (string-prefix? (path->string (car files)) "tool-run-"))))

(test-case "rotating pool keeps only 10 files"
  (with-test-dir dir
                 ;; Generate 14 overflow writes
                 (for ([i (in-range 14)])
                   (define big (make-big-bytes (+ MAX-OUTPUT-BYTES (* (add1 i) 100))))
                   (truncate-output-with-overflow big "rotate-test")
                   (sleep 0.002)) ; ensure different timestamps
                 ;; Should keep only 10
                 (define files (directory-list dir))
                 (check = (length files) 10)))

(test-case "output-overflow-dir can be overridden"
  (with-test-dir dir1
                 (define inner (build-path dir1 "sub"))
                 (make-directory inner)
                 (parameterize ([output-overflow-dir inner])
                   (define big (make-lines (+ MAX-OUTPUT-LINES 100)))
                   (define result (truncate-output-with-overflow big))
                   (check-true (string-contains? result (path->string inner)))
                   (check-equal? (length (directory-list inner)) 1))))

(test-case "notice appended after truncation notice"
  (with-test-dir dir
                 (define big (make-lines (+ MAX-OUTPUT-LINES 500)))
                 (define result (truncate-output-with-overflow big))
                 ;; Should have both truncation and overflow notices
                 (check-true (string-contains? result "[Output truncated."))
                 (check-true (string-contains? result "[Full output saved to"))
                 ;; Overflow notice should come after truncation notice
                 (define trunc-idx (caar (regexp-match-positions #rx"\\[Output truncated\\." result)))
                 (define overflow-idx
                   (caar (regexp-match-positions #rx"\\[Full output saved to" result)))
                 (check-true (< trunc-idx overflow-idx))))

(test-case "byte-limit overflow saves file and reports total bytes"
  (with-test-dir dir
                 (define big (make-big-bytes (+ MAX-OUTPUT-BYTES 2000)))
                 (define result (truncate-output-with-overflow big "bytes-test"))
                 (check-true (string-contains? result "total bytes"))
                 (check-true (string-contains? result "bytes-test-"))
                 ;; Overflow file has full content
                 (define files (directory-list dir))
                 (check = (length files) 1)
                 (check-equal? (file->string (build-path dir (car files))) big)))
