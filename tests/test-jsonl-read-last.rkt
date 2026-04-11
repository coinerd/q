#lang racket

(require rackunit
         json
         racket/file
         racket/port
         "../util/jsonl.rkt")

;; Helper: create a temp JSONL file, run body, clean up.
(define (with-temp-jsonl entries body-fn)
  (define tmp (make-temporary-file "jsonl-test-~a.jsonl"))
  (dynamic-wind
    (lambda ()
      (for ([e (in-list entries)])
        (jsonl-append! tmp e)))
    (lambda ()
      (body-fn tmp))
    (lambda ()
      (when (file-exists? tmp)
        (delete-file tmp)))))

;; Helper: write raw content (not through jsonl-append!) for partial/corrupt tests
(define (with-raw-temp-file content body-fn)
  (define tmp (make-temporary-file "jsonl-raw-test-~a.jsonl"))
  (dynamic-wind
    (lambda ()
      (call-with-output-file tmp
        (lambda (out) (display content out))
        #:mode 'text
        #:exists 'truncate))
    (lambda ()
      (body-fn tmp))
    (lambda ()
      (when (file-exists? tmp)
        (delete-file tmp)))))

;; ============================================================
;; jsonl-read-last — basic behaviour
;; ============================================================

(test-case "jsonl-read-last returns last N entries from multi-entry file"
  (define entries (for/list ([i (in-range 10)])
                    (hash 'index i 'label (format "entry~a" i))))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 3))
      (check-equal? (length result) 3)
      (check-equal? (hash-ref (first result) 'index) 7)
      (check-equal? (hash-ref (second result) 'index) 8)
      (check-equal? (hash-ref (third result) 'index) 9))))

(test-case "jsonl-read-last returns all when max-lines > file size"
  (define entries (list (hash 'a 1) (hash 'b 2) (hash 'c 3)))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 100))
      (check-equal? (length result) 3)
      (check-equal? (hash-ref (first result) 'a) 1)
      (check-equal? (hash-ref (second result) 'b) 2)
      (check-equal? (hash-ref (third result) 'c) 3))))

(test-case "jsonl-read-last returns single entry when file has one line"
  (with-temp-jsonl (list (hash 'only #t))
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? (length result) 1)
      (check-equal? (hash-ref (first result) 'only) #t))))

(test-case "jsonl-read-last returns empty list for nonexistent file"
  (define result (jsonl-read-last "/tmp/no-such-file-jsonl-test-xyz.jsonl" 5))
  (check-equal? result '()))

(test-case "jsonl-read-last with empty file returns empty list"
  (with-raw-temp-file ""
    (lambda (tmp)
      (define result (jsonl-read-last tmp 5))
      (check-equal? result '()))))

(test-case "jsonl-read-last with file of only newlines returns empty list"
  (with-raw-temp-file "\n\n\n\n"
    (lambda (tmp)
      (define result (jsonl-read-last tmp 5))
      (check-equal? result '()))))

;; ============================================================
;; jsonl-read-last — partial / corrupted lines
;; ============================================================

(test-case "jsonl-read-last handles file with no trailing newline"
  ;; Write valid JSON without trailing newline
  (with-raw-temp-file "{\"a\":1}\n{\"b\":2}"
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      ;; Both lines should parse (the last one lacks \n but is valid JSON)
      (check-equal? (length result) 2)
      (check-equal? (hash-ref (first result) 'a) 1)
      (check-equal? (hash-ref (second result) 'b) 2))))

(test-case "jsonl-read-last skips invalid JSON lines"
  (with-raw-temp-file "{\"good\":1}\nTHIS IS NOT JSON\n{\"good\":2}\n"
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? (length result) 2)
      (check-equal? (hash-ref (first result) 'good) 1)
      (check-equal? (hash-ref (second result) 'good) 2))))

(test-case "jsonl-read-last handles file with truncated JSON at end"
  (with-raw-temp-file "{\"ok\":1}\n{\"broken"
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      ;; Only the first valid line should be returned
      (check-equal? (length result) 1)
      (check-equal? (hash-ref (first result) 'ok) 1))))

(test-case "jsonl-read-last with all-corrupted lines returns empty list"
  (with-raw-temp-file "not json\nalso not json\n{broken\n"
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? result '()))))

;; ============================================================
;; jsonl-read-last — max-lines edge cases
;; ============================================================

(test-case "jsonl-read-last with max-lines 0 returns empty list"
  (with-temp-jsonl (list (hash 'a 1) (hash 'b 2))
    (lambda (tmp)
      (define result (jsonl-read-last tmp 0))
      (check-equal? result '()))))

(test-case "jsonl-read-last with max-lines 1 returns only last entry"
  (define entries (for/list ([i (in-range 5)])
                    (hash 'i i)))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 1))
      (check-equal? (length result) 1)
      (check-equal? (hash-ref (first result) 'i) 4))))

(test-case "jsonl-read-last with default max-lines (1000) returns up to 1000"
  (define entries (for/list ([i (in-range 50)])
                    (hash 'i i)))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp))
      (check-equal? (length result) 50)
      ;; First should be index 0, last index 49
      (check-equal? (hash-ref (first result) 'i) 0)
      (check-equal? (hash-ref (last result) 'i) 49))))

;; ============================================================
;; jsonl-read-last — data variety
;; ============================================================

(test-case "jsonl-read-last preserves JSON types: strings, numbers, booleans, null, arrays"
  (define entries
    (list (hash 's "hello" 'n 42 'b #t 'u (json-null) 'arr '(1 2 3))))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? (length result) 1)
      (define e (first result))
      (check-equal? (hash-ref e 's) "hello")
      (check-equal? (hash-ref e 'n) 42)
      (check-equal? (hash-ref e 'b) #t)
      (check-equal? (hash-ref e 'u) (json-null))
      (check-equal? (hash-ref e 'arr) '(1 2 3)))))

(test-case "jsonl-read-last handles nested JSON objects"
  (define entries
    (list (hash 'nested (hash 'inner (hash 'deep "value")))))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? (length result) 1)
      (check-equal?
       (hash-ref (hash-ref (hash-ref (first result) 'nested) 'inner) 'deep)
       "value"))))

;; ============================================================
;; jsonl-read-last — large file performance (doesn't read whole file)
;; ============================================================

(test-case "jsonl-read-last on large file returns only last max-lines entries"
  ;; Create a file with many entries, request fewer
  (define N 200)
  (define entries (for/list ([i (in-range N)])
                    (hash 'index i)))
  (with-temp-jsonl entries
    (lambda (tmp)
      (define result (jsonl-read-last tmp 10))
      (check-equal? (length result) 10)
      ;; Should be the last 10: indices 190-199
      (check-equal? (hash-ref (first result) 'index) 190)
      (check-equal? (hash-ref (last result) 'index) 199))))

;; ============================================================
;; jsonl-line-valid? — companion function tests
;; ============================================================

(test-case "jsonl-line-valid? accepts valid JSON objects"
  (check-true (jsonl-line-valid? "{\"a\":1}"))
  (check-true (jsonl-line-valid? "42"))
  (check-true (jsonl-line-valid? "\"hello\""))
  (check-true (jsonl-line-valid? "true"))
  (check-true (jsonl-line-valid? "null"))
  (check-true (jsonl-line-valid? "[1,2,3]")))

(test-case "jsonl-line-valid? rejects invalid JSON"
  (check-false (jsonl-line-valid? "not json"))
  (check-false (jsonl-line-valid? "{broken"))
  (check-false (jsonl-line-valid? "")))

(test-case "jsonl-line-valid? rejects whitespace-only strings"
  (check-false (jsonl-line-valid? "   "))
  (check-false (jsonl-line-valid? "\t\n")))
