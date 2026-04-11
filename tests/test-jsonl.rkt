#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../util/jsonl.rkt")

;; Helper: create a temp file path, clean up after test
(define (with-temp-file proc)
  (define path (make-temporary-file "q-jsonl-test-~a.tmp"))
  (dynamic-wind
    (lambda () (void))
    (lambda () (proc path))
    (lambda ()
      (when (file-exists? path)
        (delete-file path)))))

(define (with-temp-dir proc)
  (define dir (make-temporary-file "q-jsonl-dir-~a"))
  (delete-file dir)
  (make-directory dir)
  (dynamic-wind
    (lambda () (void))
    (lambda () (proc dir))
    (lambda ()
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define jsonl-suite
  (test-suite
   "jsonl tests"

   ;; --- jsonl-line-valid? ---

   (test-case "jsonl-line-valid? accepts valid JSON objects"
     (check-true (jsonl-line-valid? "{\"a\":1}"))
     (check-true (jsonl-line-valid? "{\"name\":\"test\",\"value\":42}"))
     (check-true (jsonl-line-valid? "null"))
     (check-true (jsonl-line-valid? "42"))
     (check-true (jsonl-line-valid? "\"hello\""))
     (check-true (jsonl-line-valid? "[1,2,3]")))

   (test-case "jsonl-line-valid? rejects invalid/incomplete JSON"
     (check-false (jsonl-line-valid? ""))
     (check-false (jsonl-line-valid? "{\"a\":"))
     (check-false (jsonl-line-valid? "{\"a\":1"))
     (check-false (jsonl-line-valid? "not json"))
     (check-false (jsonl-line-valid? "{trailing")))

   ;; --- jsonl-append! ---

   (test-case "jsonl-append! creates file if missing"
     (with-temp-dir
      (lambda (dir)
        (define path (build-path dir "test.jsonl"))
        (check-false (file-exists? path))
        (jsonl-append! path (hasheq 'hello "world"))
        (check-true (file-exists? path)))))

   (test-case "jsonl-append! appends a single entry"
     (with-temp-file
      (lambda (path)
        (jsonl-append! path (hasheq 'id "1" 'role "user"))
        (define lines (file->lines path))
        (check-equal? (length lines) 1)
        (check-equal? (read-json (open-input-string (car lines)))
                      (hasheq 'id "1" 'role "user")))))

   (test-case "jsonl-append! appends multiple entries as separate lines"
     (with-temp-file
      (lambda (path)
        (jsonl-append! path (hasheq 'n 1))
        (jsonl-append! path (hasheq 'n 2))
        (jsonl-append! path (hasheq 'n 3))
        (define lines (file->lines path))
        (check-equal? (length lines) 3))))

   ;; --- jsonl-read-all ---

   (test-case "jsonl-read-all reads all entries"
     (with-temp-file
      (lambda (path)
        (jsonl-append! path (hasheq 'id "a"))
        (jsonl-append! path (hasheq 'id "b"))
        (define entries (jsonl-read-all path))
        (check-equal? (length entries) 2)
        (check-equal? (hash-ref (car entries) 'id) "a")
        (check-equal? (hash-ref (cadr entries) 'id) "b"))))

   (test-case "jsonl-read-all returns empty list for missing file"
     (with-temp-dir
      (lambda (dir)
        (define path (build-path dir "nonexistent.jsonl"))
        (check-equal? (jsonl-read-all path) '()))))

   (test-case "jsonl-read-all throws on corrupted line"
     (with-temp-file
      (lambda (path)
        ;; Write valid then invalid
        (call-with-output-file path
          (lambda (out)
            (write-string "{\"a\":1}\n" out)
            (write-string "BROKEN\n" out))
          #:exists 'replace)
        (check-exn exn:fail?
                   (lambda () (jsonl-read-all path))))))

   ;; --- jsonl-read-all-valid ---

   (test-case "jsonl-read-all-valid skips partial/corrupted lines"
     (with-temp-file
      (lambda (path)
        (call-with-output-file path
          (lambda (out)
            (write-string "{\"a\":1}\n" out)
            (write-string "BROKEN\n" out)
            (write-string "{\"b\":2}\n" out)
            (write-string "{\"c\":\n" out))  ;; incomplete JSON
          #:exists 'replace)
        (define entries (jsonl-read-all-valid path))
        (check-equal? (length entries) 2)
        (check-equal? (hash-ref (car entries) 'a) 1)
        (check-equal? (hash-ref (cadr entries) 'b) 2))))

   (test-case "jsonl-read-all-valid returns empty for missing file"
     (with-temp-dir
      (lambda (dir)
        (define path (build-path dir "nonexistent.jsonl"))
        (check-equal? (jsonl-read-all-valid path) '()))))

   (test-case "jsonl-read-all-valid handles empty file"
     (with-temp-file
      (lambda (path)
        ;; File exists but is empty
        (call-with-output-file path void #:exists 'replace)
        (check-equal? (jsonl-read-all-valid path) '()))))

   (test-case "jsonl-read-all-valid skips trailing empty lines"
     (with-temp-file
      (lambda (path)
        (call-with-output-file path
          (lambda (out)
            (write-string "{\"x\":1}\n\n\n" out))
          #:exists 'replace)
        (define entries (jsonl-read-all-valid path))
        (check-equal? (length entries) 1))))

   ;; --- jsonl-append-entries! (atomic) ---

   (test-case "jsonl-append-entries! appends all entries atomically"
     (with-temp-file
      (lambda (path)
        (define entries
          (list (hasheq 'id "1" 'role "user")
                (hasheq 'id "2" 'role "assistant")
                (hasheq 'id "3" 'role "user")))
        (jsonl-append-entries! path entries)
        (define result (jsonl-read-all path))
        (check-equal? (length result) 3)
        (check-equal? (hash-ref (car result) 'id) "1")
        (check-equal? (hash-ref (cadr result) 'id) "2")
        (check-equal? (hash-ref (caddr result) 'id) "3"))))

   (test-case "jsonl-append-entries! creates file if missing"
     (with-temp-dir
      (lambda (dir)
        (define path (build-path dir "new.jsonl"))
        (jsonl-append-entries! path (list (hasheq 'hello "world")))
        (check-pred file-exists? path)
        (define result (jsonl-read-all path))
        (check-equal? (length result) 1))))

   (test-case "jsonl-append-entries! with empty list does nothing"
     (with-temp-dir
      (lambda (dir)
        (define path (build-path dir "empty.jsonl"))
        (jsonl-append-entries! path '())
        (check-false (file-exists? path)))))

   ;; --- Roundtrip ---

   (test-case "roundtrip: write and read preserves all types"
     (with-temp-file
      (lambda (path)
        (define entry (hasheq 'string "hello"
                              'number 42
                              'float 3.14
                              'bool #t
                              'null 'null
                              'list '(1 2 3)))
        (jsonl-append! path entry)
        (define result (jsonl-read-all path))
        (check-equal? (length result) 1)
        (define r (car result))
        (check-equal? (hash-ref r 'string) "hello")
        (check-equal? (hash-ref r 'number) 42)
        (check-equal? (hash-ref r 'bool) #t)
        (check-equal? (hash-ref r 'list) '(1 2 3)))))

   ;; --- Crash recovery simulation ---

   (test-case "partial write at end of file is detectable"
     (with-temp-file
      (lambda (path)
        ;; Write valid entries
        (jsonl-append! path (hasheq 'id "1"))
        (jsonl-append! path (hasheq 'id "2"))
        ;; Simulate partial write by truncating last line
        (define content (file->string path))
        (call-with-output-file path
          (lambda (out)
            (write-string (string-trim content) out)
            (write-string "\n{\"broken" out))  ;; partial JSON at end
          #:exists 'replace)
        ;; jsonl-read-all-valid should skip the partial line
        (define valid (jsonl-read-all-valid path))
        (check-equal? (length valid) 2)
        (check-equal? (hash-ref (car valid) 'id) "1")
        (check-equal? (hash-ref (cadr valid) 'id) "2"))))
   ))

(run-tests jsonl-suite 'verbose)
