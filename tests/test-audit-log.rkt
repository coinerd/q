#lang racket

;; tests/test-audit-log.rkt — tests for util/audit-log.rkt

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         "../util/audit-log.rkt")

;; ============================================================
;; Helper: create a temp file for the audit log
;; ============================================================

(define (make-temp-audit-path)
  (define dir (make-temporary-file "audit-test-~a" 'directory))
  (build-path dir "audit.log"))

(define (with-temp-log test-thunk)
  (define tmp-path (make-temp-audit-path))
  (dynamic-wind
    (lambda () (void))
    (lambda () (test-thunk tmp-path))
    (lambda ()
      (when (file-exists? tmp-path)
        (delete-file tmp-path))
      (define dir (let-values ([(base _name _must-be-dir?) (split-path tmp-path)])
                    base))
      (when (directory-exists? dir)
        (delete-directory dir)))))

;; ============================================================
;; Test: audit-log! writes to a temp file
;; ============================================================

(test-case
 "audit-log! writes to temp file"
 (with-temp-log
  (lambda (tmp-path)
    (audit-log! "sess-001" 'file-read "/etc/passwd" #:log-path tmp-path)
    (check-true (file-exists? tmp-path) "audit log file should exist")
    (define content (file->string tmp-path))
    (check-not-equal? content "" "audit log should not be empty"))))

;; ============================================================
;; Test: format is correct (timestamp session-id action path)
;; ============================================================

(test-case
 "audit-log! format is correct"
 (with-temp-log
  (lambda (tmp-path)
    (audit-log! "sess-123" 'file-read "/etc/passwd" #:log-path tmp-path)
    (define content (file->string tmp-path))
    ;; Format: TIMESTAMP SESSION-ID ACTION PATH\n
    (define parts (string-split (string-trim content) " "))
    (check-equal? (length parts) 4 "should have exactly 4 fields")
    (check-true (regexp-match? #rx"[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" (first parts))
                (format "timestamp should be ISO-8601, got: ~a" (first parts)))
    (check-equal? (second parts) "sess-123" "session-id should match")
    (check-equal? (third parts) "file-read" "action should match")
    (check-equal? (fourth parts) "/etc/passwd" "path should match"))))

;; ============================================================
;; Test: append mode (multiple entries)
;; ============================================================

(test-case
 "audit-log! appends multiple entries"
 (with-temp-log
  (lambda (tmp-path)
    (audit-log! "sess-001" 'file-read "/etc/hosts" #:log-path tmp-path)
    (audit-log! "sess-001" 'file-write "/tmp/test.txt" #:log-path tmp-path)
    (audit-log! "sess-002" 'file-read "/var/log/syslog" #:log-path tmp-path)
    (define lines (string-split (string-trim (file->string tmp-path)) "\n"))
    (check-equal? (length lines) 3 "should have exactly 3 lines")
    ;; Check each entry has 4 fields
    (for ([line (in-list lines)])
      (define parts (string-split line " "))
      (check-equal? (length parts) 4
                    (format "each line should have 4 fields, got: ~a" parts))))))

;; ============================================================
;; Test: with-audit-log returns thunk result
;; ============================================================

(test-case
 "with-audit-log returns thunk result"
 (with-temp-log
  (lambda (tmp-path)
    (define result
      (with-audit-log "sess-001" 'file-read "/etc/passwd"
        (lambda () 42)
        #:log-path tmp-path))
    (check-equal? result 42 "with-audit-log should return thunk result")
    (define content (file->string tmp-path))
    (check-not-equal? content "" "audit log should have been written"))))

;; ============================================================
;; Run all tests
;; ============================================================

;; ============================================================
;; SEC-04: Audit log rotation tests
;; ============================================================

(test-case
 "SEC-04: rotation happens when file exceeds max size"
 (let* ([tmpdir (make-temporary-file "audit-rot-~a" 'directory)]
        [tmp-path (build-path tmpdir "audit.log")])
   ;; Write initial content larger than 100 bytes
   (call-with-output-file tmp-path
     (lambda (out)
       (display (make-string 200 #\X) out))
     #:exists 'replace)
   ;; Set a small max size to trigger rotation
   (parameterize ([current-audit-log-max-bytes 100])
     (audit-log! "sess-rot" 'file-read (build-path "test" "path") #:log-path tmp-path))
   ;; The .1 backup should exist
   (define backup-path (path-replace-suffix tmp-path ".1"))
   (check-true (file-exists? backup-path) "rotated backup should exist")
   ;; The main log should have been recreated with the new entry
   (check-true (file-exists? tmp-path) "main log should still exist")
   (check-true (string-contains? (file->string tmp-path) "sess-rot") "new entry should be in log")
   (delete-directory/files tmpdir)))

(test-case
 "SEC-04: small entries don't trigger rotation"
 (let* ([tmpdir (make-temporary-file "audit-norot-~a" 'directory)]
        [tmp-path (build-path tmpdir "audit.log")])
   ;; Write a small entry
   (parameterize ([current-audit-log-max-bytes 10485760])
     (audit-log! "sess-small" 'file-write "/small" #:log-path tmp-path))
   ;; No .1 backup should exist
   (define backup-path (path-replace-suffix tmp-path ".1"))
   (check-false (file-exists? backup-path) "no rotation should happen")
   (delete-directory/files tmpdir)))

(void)
