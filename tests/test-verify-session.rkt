#lang racket

;; test-verify-session.rkt — Tests for q verify-session CLI command
;;
;; Issue #1288: GAP-03b — Add session integrity verification command

(require rackunit
         racket/file
         racket/path
         racket/list
         (only-in "../util/jsonl.rkt" jsonl-append!)
         (only-in "../util/protocol-types.rkt" make-message make-text-part message->jsexpr)
         (only-in "../runtime/session-store.rkt"
                  append-entry!
                  verify-hash-chain
                  verify-session-integrity
                  repair-session-log!))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-tmp-dir)
  (define tmp (make-temporary-file "verify-session-test-~a"))
  (delete-file tmp)
  (make-directory tmp)
  tmp)

(define (cleanup tmp)
  (when (directory-exists? tmp)
    (delete-directory/files tmp)))

(define (make-test-msg [n 0])
  (make-message (format "msg-~a-~a" n (current-milliseconds))
                #f
                'user
                'text
                (list (make-text-part (format "Test ~a" n)))
                (+ 1000 n)
                (hasheq)))

;; ---------------------------------------------------------------------------
;; Tests: verify-session-integrity with hash chain
;; ---------------------------------------------------------------------------

(test-case "verify-session: structural integrity passes for valid log"
  (define tmp (make-tmp-dir))
  (define log-path (build-path tmp "session.jsonl"))
  (for ([i (in-range 5)])
    (append-entry! log-path (make-test-msg i)))
  (define report (verify-session-integrity log-path))
  (check-equal? (hash-ref report 'total-entries) 5)
  (check-equal? (hash-ref report 'valid-entries) 5)
  (check-equal? (hash-ref report 'invalid-entries) '())
  (check-equal? (hash-ref report 'truncated-at-end?) #f)
  (check-equal? (hash-ref report 'entry-order-valid?) #t)
  (cleanup tmp))

(test-case "verify-session: hash chain + structure both pass"
  (define tmp (make-tmp-dir))
  (define log-path (build-path tmp "session.jsonl"))
  (for ([i (in-range 3)])
    (append-entry! log-path (make-test-msg i)))
  (define chain-report (verify-hash-chain log-path))
  (define struct-report (verify-session-integrity log-path))
  (check-equal? (hash-ref chain-report 'valid?) #t)
  (check-equal? (hash-ref struct-report 'valid-entries) 3)
  (cleanup tmp))

(test-case "verify-session: detects invalid JSON lines"
  (define tmp (make-tmp-dir))
  (define log-path (build-path tmp "session.jsonl"))
  (append-entry! log-path (make-test-msg 0))
  ;; Append invalid JSON
  (call-with-output-file log-path (λ (out) (displayln "NOT JSON" out)) #:exists 'append)
  (append-entry! log-path (make-test-msg 1))
  (define report (verify-session-integrity log-path))
  (check-equal? (hash-ref report 'total-entries) 3)
  (check-true (> (hash-ref report 'valid-entries) 0))
  (check-true (> (length (hash-ref report 'invalid-entries)) 0))
  (cleanup tmp))

(test-case "verify-session: repair removes invalid and fixes chain"
  (define tmp (make-tmp-dir))
  (define log-path (build-path tmp "session.jsonl"))
  (append-entry! log-path (make-test-msg 0))
  ;; Inject invalid line
  (call-with-output-file log-path (λ (out) (displayln "{bad" out)) #:exists 'append)
  (append-entry! log-path (make-test-msg 1))
  ;; Repair
  (define rr (repair-session-log! log-path))
  (check-true (>= (hash-ref rr 'entries-removed) 1))
  ;; Post-repair chain should be valid
  (define chain-report (verify-hash-chain log-path))
  (check-equal? (hash-ref chain-report 'valid?) #t)
  (cleanup tmp))

(test-case "verify-session: legacy log without hashes returns exit code 2 semantics"
  (define tmp (make-tmp-dir))
  (define log-path (build-path tmp "session.jsonl"))
  ;; Write legacy entries without hash fields
  (for ([i (in-range 3)])
    (jsonl-append!
     log-path
     (hash 'id (format "legacy-~a" i) 'role "user" 'kind "text" 'content '() 'timestamp (+ 1000 i))))
  (define chain-report (verify-hash-chain log-path))
  (check-equal? (hash-ref chain-report 'has-hashes?) #f)
  ;; Structurally valid though
  (define struct-report (verify-session-integrity log-path))
  (check-equal? (hash-ref struct-report 'valid-entries) 3)
  (cleanup tmp))
